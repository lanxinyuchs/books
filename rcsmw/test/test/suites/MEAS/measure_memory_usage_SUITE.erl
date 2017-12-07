%%% #0.    BASIC INFORMATION
%% coding: latin-1
%%% ----------------------------------------------------------
%%% %CCaseFile:	measure_memory_usage_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R5A/2
%%%
%%% @doc == Measure and check memory size when several users using netconf and cli operations.==
%%% This Test Suite can be used on target and simulated enviroment.<br/>
%%%
%%%
%%% @end

-module(measure_memory_usage_SUITE).

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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2013-01-17 etxivri     Created
%%% R2A/2      2013-01-21 etxivri     Some updtates
%%% R2A/3      2013-01-22 etxivri     Major updates. Added also evaluate mem size.
%%% R2A/5      2013-01-22 etxivri     Corrected a minor fault.
%%% R2A/6      2013-02-18 etxivri     Added measure memory when generate and cease alarms.
%%% R2A/7      2013-03-01 etxivri     Updates of alarm handling and start to do TCs to be runed longer time.
%%%                                   10 hours is not ready to be runed yet, due to large log-files.
%%%                                   Updates of get memorysize.
%%%                                   Removed two tc than was unused.
%%% R2A/9      2013-03-01 etxivri     Remove ct:pal to remove printouts in testlog, renamed testlogfiles.
%%%                                   stored in test log path. Added rct_core hook.
%%% R2A/10     2013-04-10 etxjovp     Add meas_check_memory_nc_cli_users_add_delete_rbsUnit_150_hour.
%%% R2A/10     2013-04-11 etxivri     Added prinout and removed evaluate check in nc_cli_users_add_delete loop.
%%% R2A/12     2013-04-12 etxivri     Updates for xxl test
%%% R2A/12     2013-04-18 etxivri     More Updates for xxl test, added mem size for tot_bc.
%%% R2A/13     2013-04-24 etxivri     More Updates for xxl test, added a print answer from top cmd.
%%% R2A/17     2013-04-30 etxivri     More Updates for xxl test, added a print cpu background load.
%%% R2A/18     2013-05-13 etxivri     More Updates for xxl test, added timeout in nc operations.
%%% R2A/19     2013-05-14 etxivri     More Updates for xxl test, correct usage of timeout in nc open session.
%%% R2A/20     2013-05-27 etxivri     Update check that alarm is not active, cli_check_alarm_is_inactive.
%%% R2A/21     2013-06-13 etxivri     Created a TC, paralell nc operations 10 hours. Not ready to be used yet.
%%% R2A/22     2013-06-24 etxivri     Change in delete snmp config.
%%% R2A/23     2013-08-07 etxivri     Simplified check of snmp config created, and decreased numbers of snmp targets.
%%%                                   Updates due to one snmpd pid exist after installation.
%%% R2A/24     2013-08-13 etxkols     Changed raw to no_prompt_check in ct_telnet:expect.
%%% R2A/25     2013-08-16 etxivri     More Updates for xxl test, added print of nr of process and cpulimit used mem size.
%%% R2A/26     2013-08-20 etxivri     More Updates for xxl test.
%%% R2A/26     2013-08-21 etxivri     More Updates for xxl test.
%%% R2A/28     2013-09-02 etxivri     Increased timeout value in netconf operations.
%%% R2A/29     2013-09-11 etxivri     Fixed a copy paste fault in xxl TC.
%%% R2A/30     2013-10-01 etxivri     Changed cli command end to top, due to changes in com bd8.
%%% R2A/31     2013-11-05 etxivri     New XXL TC for TCU board, Not using cpulimit.
%%% R2A/32     2013-11-05 etxivri     Updated XXL TC that cpulimit on arm architecture is also used.
%%% R2A/33     2014-01-16 etxkols     Support for dus5201.
%%% R5A/1      2016-01-21 erarafo     Fixed deprecation warnings and copyright
%%% R5A/2      2016-02-17 etxkols     Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
%%% ----------------------------------------------------------
%%%

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 groups/0,
	 all/0,
	 meas_check_memory_nc_cli_users_add_delete_rbsUnit_1_hour/1,
	 meas_check_memory_nc_cli_users_add_delete_rbsUnit_10_hour/1,
	 meas_check_memory_nc_cli_users_add_delete_rbsUnit_150_hour/1,
	 meas_check_memory_nc_cli_users_add_delete_rbsUnit_150_hour_tcu/1,
	 meas_check_memory_paralell_nc_add_delete_rbsUnit_10_hour/1,
	 meas_check_memory_alarm_generate_cease_1_hour/1,
	 meas_check_memory_alarm_generate_cease_10_hour/1
	]).

%% -define(NrOfNetConfUsers, 1).
%% -define(NrOfCliUsers, 1).
-define(NrOfNetConfUsers, 20).
-define(NrOfCliUsers, 20).

-define(NC_SessionNameList, [list_to_atom("nc"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfNetConfUsers)]).
-define(CLI_SessionNameList, [list_to_atom("cli"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfCliUsers)]).

%% -define(All_MEASURED_FILE, "add_delete_rbsUnits_measured_memory_file.txt").
-define(All_NC_CLI_OPERATION_MEASURED_FILE, "all_nc_cli_operation_measured_memory_file.txt").
-define(All_NC_CLI_OPERATION_MEASURED_FILE_2, "all_nc_cli_operation_measured_memory_file_2.txt").
%% -define(ALARM_MEASURED_FILE, "alarm_measured_memory_file.txt").
-define(ALL_ALARM_MEASURED_FILE, "all_alarm_measured_memory_file.txt").
-define(All_MEASURED_LOGS, "all_measured_memory_log.txt").

-define(LOG_DIR, "/proj/rcs/measurements/").
%% -define(LOG_DIR, "/home/etxivri/tmp/").


-define(CPULIMIT_FROM_PATH, " /proj/rcs/misc/cpulimit").
-define(CPULIMIT_FROM_PATH_ARM, " /proj/rcs/misc/cpulimit_arm/cpulimit").
-define(CPULIMIT_TO_PATH, "/home/sirpa/dev_patches/").

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% runs the rct_netconf hook for each user, to be able to open a netconf session for each user.<br/>
%% @end
%%--------------------------------------------------------------------
suite() ->
    %% build up a list of NetconfHooks touples with differents Names.
    NetconfHooks = [{rct_netconf, list_to_atom("nc"++integer_to_list(N))} || N <- lists:seq(1,?NrOfNetConfUsers)],
    CliHooks = [{rct_cli, {list_to_atom("cli"++integer_to_list(N)), [manual_connect]}} || N <- lists:seq(1,?NrOfCliUsers)] ,

    %% ct:pal("CliHooks: ~p",[CliHooks]),
    [{timetrap, {hours, 200}}, % 200 hours
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 %% {cth_conn_log, []},
		 {rct_consserv,cs1},
                 {rct_rs232,console},
		 {rct_scp, [{1, node1}]},
		 {rct_core,[]},
		 {rct_tlib, identifier}, %% some miscellaneous functions
		 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}} |
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
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(_TestCase, Config) ->
    %% Check if testcase failed and clean up

    %%%%
    %% Remove background load on the CPUs, kill cat pids and cpulimit pids.
    %% Note! pkill cat before pkill cpulimit, otherwise a core dump will be done
    %%%%
    rct_rpc:call(rpc, os, cmd, ["pkill cat"], 10000, noprint),
    timer:sleep(2000),    %% sleep neede, otherwise a core dump will be done
    rct_rpc:call(rpc, os, cmd, ["pkill cpulimit"], 10000, noprint),
    rct_rpc:call(rpc, os, cmd, ["rm "++?CPULIMIT_TO_PATH++"cpulimit"], 10000, noprint),

    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p.  \nClean up added rbs units", [Reason]),

	    A = rct_rpc:call(rpc, os, cmd, ["pgrep com"], 10000, noprint),
	    B = rct_rpc:call(rpc, os, cmd, ["pgrep cli"], 10000, noprint),
	    C = rct_rpc:call(rpc, os, cmd, ["pgrep netconf"], 10000, noprint),
	    BB = length(string:tokens(B,"\n")),
	    CC = length(string:tokens(C,"\n")),
	    ct:pal("### Com: ~p",[A]),
	    ct:pal("### Cli: ~p",[BB]),
	    ct:pal("### Netconf: ~p",[CC]),

	    %%%%%%%%%%%%%%
	    %% Clean up Netconf configurations!
	    %%%%%%%%%%%%%%
	    %%%%
	    %% Open a netconf session and clean up rbsUnits, close all session.
            %%%%
	    try
	    	lists:foreach(fun(Name) -> nc_open(ct_netconfc:open(Name, []), Name) end, ?NC_SessionNameList)
	    catch throw:{?MODULE, found} ->
	    	    ok
	    end,

            %%%%
	    %% Commit
            %%%%
	    lists:foreach(fun(Name1) ->
	    			  %% ct:pal("### Cleanup after fail TC, Close session: ~p", [Name1]),
	    			  ct_netconfc:close_session(Name1)
	    		  end,
	    		  ?NC_SessionNameList),

	    %%%%%%%%%%%%%%
	    %% Clean up cli configurations!
	    %%%%%%%%%%%%%%
	    %%%%
	    %% Open a cli connection and clean up rbsUnits.
            %%%%
	    try
	    	lists:foreach(fun(Name2) -> cli_connect(rct_cli:connect(Name2, noprint), Name2) end, ?CLI_SessionNameList)
	    catch throw:{?MODULE, found} ->
	    	    ok
	    end,

	    %%%%
	    %% Close connection
	    %%%%
	    lists:foreach(fun(Name3) ->
	    			  %% ct:pal("### Cleanup after fail TC, CloseName: ~p", [Name3]),
	    			  rct_cli:disconnect(Name3, noprint)
	    		  end,
	    		  ?CLI_SessionNameList),

            %%%%
	    %% Cease alarm.
            %%%%
	    %% rct_rpc:call(rpc, lih_fm, cancel_emergency_unlock_alarm, [], 1000, noprint),
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

nc_open({error,{connection_exists,_}}, Name) ->
    ct_netconfc:close_session(Name), % Clean up at our client, if netconf process was killed on node,
    ct_netconfc:open(Name, []), % Then set up again.
    %% nc_delete_rbsunits(Name, ?NC_SessionNameList),
    nc_delete_rbsunits_no_check(Name, ?NC_SessionNameList),
    throw({?MODULE, found});
nc_open({ok,_}, Name) ->
    %% nc_delete_rbsunits(Name, ?NC_SessionNameList),
    nc_delete_rbsunits_no_check(Name, ?NC_SessionNameList),
    throw({?MODULE, found});
nc_open(_, _) ->
    ok.

cli_connect({error,already_connected}, Name) ->
    rct_cli:disconnect(Name, noprint), % Clean up at our client, if cli process was killed on node,
    rct_cli:connect(Name, noprint), % Then set up again.
    cli_delete_rbsUnits(Name, ?CLI_SessionNameList, noprint),
    cli_delete_conf_snmp(Name, ?CLI_SessionNameList, noprint),
    throw({?MODULE, found});
cli_connect(ok, Name) ->
    cli_delete_rbsUnits(Name, ?CLI_SessionNameList, noprint),
    cli_delete_conf_snmp(Name, ?CLI_SessionNameList, noprint),
    throw({?MODULE, found});
cli_connect(_, _) ->
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
%% calls do_meas_check_memory_nc_cli_users_add_rbsUnit with numbers of Hours that will perform the actual test.
%% @spec meas_check_memory_nc_cli_users_add_delete_rbsUnit_1_hour(Config) -> ok
%% @end
%%--------------------------------------------------------------------
meas_check_memory_nc_cli_users_add_delete_rbsUnit_1_hour(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_check_memory_nc_cli_users_add_delete_rbsUnit(1, LogPath).

meas_check_memory_nc_cli_users_add_delete_rbsUnit_10_hour(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_check_memory_nc_cli_users_add_delete_rbsUnit(10, LogPath).

meas_check_memory_nc_cli_users_add_delete_rbsUnit_150_hour(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_check_memory_nc_xxl(150, LogPath).

meas_check_memory_nc_cli_users_add_delete_rbsUnit_150_hour_tcu(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_check_memory_nc_xxl_tcu(150, LogPath).

meas_check_memory_paralell_nc_add_delete_rbsUnit_10_hour(Config) ->
    reboot(),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_check_memory_paralell_nc_xxl(10, LogPath).


reboot() ->
    %%%%
    %% reboot
    %%%%
    ct:pal("### reboot!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ok = rct_rs232:login(console),
    net_kernel:disconnect(ErlNode),
    ok = ct_telnet:send(console, "reboot"),
    {ok,_} = ct_telnet:expect(console, "du1 login", [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Sleep to be sure that com is up and ready to be used.
    %%%%
    ComPid = rct_tlib:wait_for_com_to_start(identifier),
    ct:pal("ComPid: ~p .",[ComPid]),
    timer:sleep(10000),

    ok.
%%--------------------------------------------------------------------
%% @doc
%% Check memory size after several Netconf and Cli users operations. <br/>
%% After each action os:cmd(sync) is done, to make sure disk is updated. <br/>
%% This TC log measured memory size to file.<br/>
%% - reboot node. <br/>
%% - Do nc and cli operation once.
%% - Check used memory for com, beam. This will be the starting point!<br/>
%%   ps -eo fname,vsz,rss | egrep '(beam)|(com)' <br/>
%% - For one hour. Do nc and cli operation. <br/>
%%    - Open Session. <br/>
%%    - Add rbsUnits. <br/>
%%    - Delete rbsUnits. <br/>
%%    - Close sessions. <br/>
%% - Check used memory for com, beam. This will be the end point!<br/>
%%   ps -eo fname,vsz,rss | egrep '(beam)|(com)' <br/>
%% @spec do_meas_check_memory_nc_cli_users_add_delete_rbsUnit(Hours, LogPath) -> ok
%% @end
%%--------------------------------------------------------------------
do_meas_check_memory_nc_cli_users_add_delete_rbsUnit(Hours, LogPath) ->
    ct:pal("### Check memory when several netconf and cli users, add rbs units, ~p hours!", [Hours]),
    %%%%
    %% Get sw version, using cli
    %%%%
    [H|_] = ?CLI_SessionNameList,
    CXS_label = get_sw_version(H),

    Seconds = Hours*60*60,

    [{_, NodeName}] = ct:get_config(test_nodes),

    %%%%
    %% reboot
    %%%%
    ct:pal("### reboot!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ok = rct_rs232:login(console),
    net_kernel:disconnect(ErlNode),
    ok = ct_telnet:send(console, "reboot"),
    {ok,_} = ct_telnet:expect(console, "du1 login", [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Sleep to be sure that com is up and ready to be used.
    %%%%
    ComPid = rct_tlib:wait_for_com_to_start(identifier),
    ct:pal("ComPid: ~p .",[ComPid]),
    timer:sleep(10000),
    %%%%
    %% Get Beam Pid. Will be used when get memsize.
    %%%%
    BeamPid = get_correct_beam_pid(),
    ct:pal("BeamPid: ~p .",[BeamPid]),

    %%%%
    %% Open nc sessions and create cli connection.
    %%%%
    nc_open_sessions(?NC_SessionNameList),
    cli_create_connection(?CLI_SessionNameList),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    %%%%
    %% Perform NC and CLI operations.
    %% Open, add, delete close nc and cli.
    %%%%
    ct:pal("### Perform NC and CLI operation first time before get memory size,", []),
    ok = perform_nc_cli_operations_once(noprint),

    %% %% %%%%
    %% %% %% get memory size
    %% %% %%%%
    [Start_Com_rss, Start_Beam_rss] = get_used_mem_size([com, beam], BeamPid),

    %%%%
    %% For one hour.
    %% Perform NC and CLI operations.
    %% Open, add, delete close nc and cli.
    %%%%
    StartTime = erlang:timestamp(),
    {End_Com_rss,
     End_Beam_rss,
     Nr} = loop_nc_cli_operations_check_mem(StartTime,
					    Seconds,
					    %% 60,
					    Start_Com_rss,
					    Start_Beam_rss,
					    LogPath,
					    BeamPid),

    %%%%
    %% Write info to file
    %%%%
    FileName = "meas_check_memory_nc_cli_users_add_delete_rbsUnit_"++ integer_to_list(Hours) ++"_hour",
    rct_tlib:writeDataToFile(?LOG_DIR, FileName, "~p;~w;~p;~s;~s;~p;~s;~s;~p;~w~n",
    			     [httpd_util:rfc1123_date(),
    			      CXS_label,
			      Nr,
    			      Start_Com_rss,
			      End_Com_rss,
			      list_to_integer(End_Com_rss) - list_to_integer(Start_Com_rss),
    			      Start_Beam_rss,
    			      End_Beam_rss,
			      list_to_integer(End_Beam_rss) - list_to_integer(Start_Beam_rss),
    			      NodeName
    			     ]),

    ct:pal("Start: com: ~s,  beam: ~s ~n", [Start_Com_rss,
    					    Start_Beam_rss
    					   ]),

    ct:pal("End  : com: ~s,  beam: ~s ~n", [End_Com_rss,
    					    End_Beam_rss
    					   ]),

    %%%%
    %% Close nc session and cli connections.
    %%%%
    %% ct:pal("Close nc sessions and cli connections.",[]),
    nc_close_sessions(?NC_SessionNameList),
    cli_remove_connection(?CLI_SessionNameList, noprint),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    %%%%
    %% check nc sessions deleted
    %% check cli connection deleted
    %%%%
    ct:pal("Check nc session and cli connections is deleted.",[]),
    [] = rct_rpc:call(rpc, os, cmd, ["pgrep netconf"], 10000, noprint),
    [] = rct_rpc:call(rpc, os, cmd, ["pgrep cli"], 10000, noprint),

    ct:pal("All measured memory is loged at:",[]),
    ct:pal("LogPath: ~p, filename:  ~p ",[LogPath, ?All_NC_CLI_OPERATION_MEASURED_FILE]),

    ok.

%%%%%%%%%%%%
%% 150 h
%%%%%%%%%%%%
do_meas_check_memory_nc_xxl(Hours, LogPath) ->
    ct:pal("### Check memory when several netconf users, add rbs units, ~p hours!", [Hours]),
    %%%%
    %% Get sw version, using cli
    %%%%
    [H|_] = ?CLI_SessionNameList,
    CXS_label = get_sw_version(H),

    Seconds = Hours*60*60,

    [{_, NodeName}] = ct:get_config(test_nodes),

    %%%%
    %% reboot
    %%%%
    ct:pal("### reboot!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ok = rct_rs232:login(console),
    net_kernel:disconnect(ErlNode),
    ok = ct_telnet:send(console, "reboot"),
    {ok,_} = ct_telnet:expect(console, "du1 login", [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Sleep to be sure that com is up and ready to be used.
    %%%%
    ComPid = rct_tlib:wait_for_com_to_start(identifier),
    ct:pal("ComPid: ~p .",[ComPid]),
    timer:sleep(10000),
    %%%%
    %% Get Beam Pid. Will be used when get memsize.
    %%%%
    BeamPid = get_correct_beam_pid(),
    ct:pal("BeamPid: ~p .",[BeamPid]),

    timer:sleep(10000),
    TestNodeBeamPid = get_testnode_beam_pid(),
    ct:pal("TestNodeBeamPid: ~p .",[TestNodeBeamPid]),

    TestOiPid = get_pid("test_oi"),
    ct:pal("TestOiPid: ~p .",[TestOiPid]),

    TestIftAppPid = get_pid("ift_app"),
    ct:pal("TestIftAppPid: ~p .",[TestIftAppPid]),

    TestAppPid = get_pid("test_app"),
    ct:pal("TestAppPid: ~p .",[TestAppPid]),

    %%%%
    %% Background load. 50% load on each CPU.
    %% Use of cpulimit to control load on each CPU.
    %%%%
    ok = generate_cpu_load("50"),
    CpuLimitPids = get_pid("cpulimit"),
    ct:pal("CpuLimitPids: ~p .",[CpuLimitPids]),

    %%%%
    %% Perform NC operations.
    %% Open, add, delete close nc.
    %%%%
    ct:pal("### Perform NC  operation first time before get memory size,", []),
    ok = perform_nc_operations_once(?NC_SessionNameList),

    ct:pal("#LogPath_1# ~p", [LogPath++?All_NC_CLI_OPERATION_MEASURED_FILE]),
    ct:pal("#LogPath_2# ~p", [LogPath++?All_NC_CLI_OPERATION_MEASURED_FILE_2]),

    %% %%%%
    %% %% get memory size
    %% %%%%
    [Start_Tot_bc,
     Start_Com_rss,
     Start_Beam_rss,
     Start_TestBeam_rss,
     Start_test_oi,
     Start_ift_app,
     Start_test_app] = get_used_mem_size_xxl(["tot_bc",
					      "com",
					      "beam",
					      "test_beam",
					      "test_oi",
					      "ift_app",
					      "test_app"], BeamPid, TestNodeBeamPid),

    %%%%
    %% For one hour.
    %% Perform NC and CLI operations.
    %% Open, add, delete close nc and cli.
    %%%%
    StartTime = os:timestamp(),
    {End_Tot_bc,
     End_Com_rss,
     End_Beam_rss,
     End_TestBeam_rss,
     End_test_oi,
     End_ift_app,
     End_test_app,
     Nr} = loop_nc_operations_check_mem(StartTime,
					Seconds,
					%% 120,
					Start_Tot_bc,
					Start_Com_rss,
					Start_Beam_rss,
					Start_TestBeam_rss,
					Start_test_oi,
					Start_ift_app,
					Start_test_app,
					LogPath,
					BeamPid,
					TestNodeBeamPid,
					CpuLimitPids),

    %%%%
    %% Write info to file
    %%%%
    FileName = "meas_check_memory_nc_cli_users_add_delete_rbsUnit_"++ integer_to_list(Hours) ++"_hour",

    rct_tlib:writeDataToFile(?LOG_DIR, FileName, "~p;~w;~p;~s;~s;~p;~s;~s;~p;~s;~s;~p;;~s;~s;~p;~s;~s;~p;~s;~s;~p;~s;~s;~p~w~n",
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
			      Start_TestBeam_rss,
			      End_TestBeam_rss,
			      list_to_integer(End_TestBeam_rss) - list_to_integer(Start_TestBeam_rss),
			      Start_test_oi,
			      End_test_oi,
			      list_to_integer(End_test_oi) - list_to_integer(Start_test_oi),
			      Start_ift_app,
			      End_test_oi,
			      list_to_integer(End_test_oi) - list_to_integer(Start_ift_app),
			      Start_test_app,
			      End_test_app,
			      list_to_integer(End_test_app) - list_to_integer(Start_test_app),
    			      NodeName
    			     ]),

    ct:pal("Start: com: ~s,  beam: ~s,  TestBeam: ~s,  test_oi: ~s,  ift_app: ~s,  test_app: ~s ~n", [Start_Com_rss,
												      Start_Beam_rss,
												      Start_TestBeam_rss,
												      Start_test_oi,
												      Start_ift_app,
												      Start_test_app
												     ]),

    ct:pal("End  : com: ~s,  beam: ~s,  TestBeam: ~s,  test_oi: ~s,  ift_app: ~s,  test_app: ~s ~n", [End_Com_rss,
												      End_Beam_rss,
												      End_TestBeam_rss,
												      End_test_oi,
												      End_ift_app,
												      End_test_app
												     ]),

    %%%%
    %% check nc sessions deleted
    %% check cli connection deleted
    %%%%
    ct:pal("Check nc session and cli connections is deleted.",[]),
    [] = rct_rpc:call(rpc, os, cmd, ["pgrep netconf"], 10000, noprint),
    [] = rct_rpc:call(rpc, os, cmd, ["pgrep cli"], 10000, noprint),

    ct:pal("All measured memory is loged at:~n"
	   "LogPath: ~p, ~n"
	   "filename 1:  ~p ~n"
	   "filename 2:  ~p ~n",[LogPath, ?All_NC_CLI_OPERATION_MEASURED_FILE, ?All_NC_CLI_OPERATION_MEASURED_FILE_2]),


    %%%%
    %% Remove background load on the CPUs, kill cat pids and cpulimit pids.
    %% Note! pkill cat before pkill cpulimit, otherwise a core dump will be done
    %%%%
    CpuLimitPidsEnd = get_pid("cpulimit"),
    ct:pal("CpuLimitPids: ~p .",[CpuLimitPidsEnd]),
    [] = rct_rpc:call(rpc, os, cmd, ["pkill cat"], 10000, noprint),
    timer:sleep(2000),    %% sleep neede, otherwise a core dump will be done
    [] = rct_rpc:call(rpc, os, cmd, ["pkill cpulimit"], 10000, noprint),

    [] = rct_rpc:call(rpc, os, cmd, ["pgrep cpulimit"], 10000, noprint),
    [] = rct_rpc:call(rpc, os, cmd, ["pgrep cat"], 10000, noprint),

    %%%%
    %% Remove cpulimit.
    %%%%
    [] = rct_rpc:call(rpc, os, cmd, ["rm "++?CPULIMIT_TO_PATH++"cpulimit"], 10000, noprint),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%
%% 150 h TCU
%%%%%%%%%%%%
do_meas_check_memory_nc_xxl_tcu(Hours, LogPath) ->
    ct:pal("### Check memory when several netconf users, add rbs units, ~p hours!", [Hours]),
    %%%%
    %% Get sw version, using cli
    %%%%
    [H|_] = ?CLI_SessionNameList,
    CXS_label = get_sw_version(H),

    Seconds = Hours*60*60,

    [{_, NodeName}] = ct:get_config(test_nodes),

    %%%%
    %% reboot
    %%%%
    ct:pal("### reboot!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ok = rct_rs232:login(console),
    net_kernel:disconnect(ErlNode),
    ok = ct_telnet:send(console, "reboot"),
    {ok,_} = ct_telnet:expect(console, "du1 login", [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Sleep to be sure that com is up and ready to be used.
    %%%%
    ComPid = rct_tlib:wait_for_com_to_start(identifier),
    ct:pal("ComPid: ~p .",[ComPid]),
    timer:sleep(10000),
    %%%%
    %% Get Beam Pid. Will be used when get memsize.
    %%%%
    BeamPid = get_correct_beam_pid(),
    ct:pal("BeamPid: ~p .",[BeamPid]),

    timer:sleep(10000),
    TestNodeBeamPid = get_testnode_beam_pid(),
    ct:pal("TestNodeBeamPid: ~p .",[TestNodeBeamPid]),

    TestOiPid = get_pid("test_oi"),
    ct:pal("TestOiPid: ~p .",[TestOiPid]),

    TestIftAppPid = get_pid("ift_app"),
    ct:pal("TestIftAppPid: ~p .",[TestIftAppPid]),

    TestAppPid = get_pid("test_app"),
    ct:pal("TestAppPid: ~p .",[TestAppPid]),

    %% %%%%
    %% %% Background load. 50% load on each CPU.
    %% %% Use of cpulimit to control load on each CPU.
    %% %%%%
    %% ok = generate_cpu_load("50"),
    %% CpuLimitPids = get_pid("cpulimit"),
    %% ct:pal("CpuLimitPids: ~p .",[CpuLimitPids]),

    %%%%
    %% Perform NC operations.
    %% Open, add, delete close nc.
    %%%%
    ct:pal("### Perform NC  operation first time before get memory size,", []),
    ok = perform_nc_operations_once(?NC_SessionNameList),

    ct:pal("#LogPath_1# ~p", [LogPath++?All_NC_CLI_OPERATION_MEASURED_FILE]),
    ct:pal("#LogPath_2# ~p", [LogPath++?All_NC_CLI_OPERATION_MEASURED_FILE_2]),

    %% %%%%
    %% %% get memory size
    %% %%%%
    [Start_Tot_bc,
     Start_Com_rss,
     Start_Beam_rss,
     Start_TestBeam_rss,
     Start_test_oi,
     Start_ift_app,
     Start_test_app] = get_used_mem_size_xxl(["tot_bc",
					      "com",
					      "beam",
					      "test_beam",
					      "test_oi",
					      "ift_app",
					      "test_app"], BeamPid, TestNodeBeamPid),

    %%%%
    %% For one hour.
    %% Perform NC and CLI operations.
    %% Open, add, delete close nc and cli.
    %%%%
    StartTime = os:timestamp(),
    {End_Tot_bc,
     End_Com_rss,
     End_Beam_rss,
     End_TestBeam_rss,
     End_test_oi,
     End_ift_app,
     End_test_app,
     Nr} = loop_nc_operations_check_mem_tcu(StartTime,
					    Seconds,
					    %% 120,
					    Start_Tot_bc,
					    Start_Com_rss,
					    Start_Beam_rss,
					    Start_TestBeam_rss,
					    Start_test_oi,
					    Start_ift_app,
					    Start_test_app,
					    LogPath,
					    BeamPid,
					    TestNodeBeamPid),

    %%%%
    %% Write info to file
    %%%%
    FileName = "meas_check_memory_nc_cli_users_add_delete_rbsUnit_"++ integer_to_list(Hours) ++"_hour",

    rct_tlib:writeDataToFile(?LOG_DIR, FileName, "~p;~w;~p;~s;~s;~p;~s;~s;~p;~s;~s;~p;;~s;~s;~p;~s;~s;~p;~s;~s;~p;~s;~s;~p~w~n",
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
			      Start_TestBeam_rss,
			      End_TestBeam_rss,
			      list_to_integer(End_TestBeam_rss) - list_to_integer(Start_TestBeam_rss),
			      Start_test_oi,
			      End_test_oi,
			      list_to_integer(End_test_oi) - list_to_integer(Start_test_oi),
			      Start_ift_app,
			      End_test_oi,
			      list_to_integer(End_test_oi) - list_to_integer(Start_ift_app),
			      Start_test_app,
			      End_test_app,
			      list_to_integer(End_test_app) - list_to_integer(Start_test_app),
    			      NodeName
    			     ]),

    ct:pal("Start: com: ~s,  beam: ~s,  TestBeam: ~s,  test_oi: ~s,  ift_app: ~s,  test_app: ~s ~n", [Start_Com_rss,
												      Start_Beam_rss,
												      Start_TestBeam_rss,
												      Start_test_oi,
												      Start_ift_app,
												      Start_test_app
												     ]),

    ct:pal("End  : com: ~s,  beam: ~s,  TestBeam: ~s,  test_oi: ~s,  ift_app: ~s,  test_app: ~s ~n", [End_Com_rss,
												      End_Beam_rss,
												      End_TestBeam_rss,
												      End_test_oi,
												      End_ift_app,
												      End_test_app
												     ]),

    %%%%
    %% check nc sessions deleted
    %% check cli connection deleted
    %%%%
    ct:pal("Check nc session and cli connections is deleted.",[]),
    [] = rct_rpc:call(rpc, os, cmd, ["pgrep netconf"], 10000, noprint),
    [] = rct_rpc:call(rpc, os, cmd, ["pgrep cli"], 10000, noprint),

    ct:pal("All measured memory is loged at:~n"
	   "LogPath: ~p, ~n"
	   "filename 1:  ~p ~n"
	   "filename 2:  ~p ~n",[LogPath, ?All_NC_CLI_OPERATION_MEASURED_FILE, ?All_NC_CLI_OPERATION_MEASURED_FILE_2]),


    %% %%%%
    %% %% Remove background load on the CPUs, kill cat pids and cpulimit pids.
    %% %% Note! pkill cat before pkill cpulimit, otherwise a core dump will be done
    %% %%%%
    %% CpuLimitPidsEnd = get_pid("cpulimit"),
    %% ct:pal("CpuLimitPids: ~p .",[CpuLimitPidsEnd]),
    %% [] = rct_rpc:call(rpc, os, cmd, ["pkill cat"], 10000, noprint),
    %% timer:sleep(2000),    %% sleep neede, otherwise a core dump will be done
    %% [] = rct_rpc:call(rpc, os, cmd, ["pkill cpulimit"], 10000, noprint),

    %% [] = rct_rpc:call(rpc, os, cmd, ["pgrep cpulimit"], 10000, noprint),
    %% [] = rct_rpc:call(rpc, os, cmd, ["pgrep cat"], 10000, noprint),

    %% %%%%
    %% %% Remove cpulimit.
    %% %%%%
    %% [] = rct_rpc:call(rpc, os, cmd, ["rm "++?CPULIMIT_TO_PATH++"cpulimit"], 10000, noprint),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%
%% Paralell XXL 10 h
%%%%%%%%%%%%
do_meas_check_memory_paralell_nc_xxl(Hours, LogPath) ->
    ct:pal("### Check memory when several netconf users, add rbs units, ~p hours!", [Hours]),
    %%%%
    %% Get sw version, using cli
    %%%%
    [H|_] = ?CLI_SessionNameList,
    CXS_label = get_sw_version(H),

    Seconds = Hours*60*60,

    [{_, NodeName}] = ct:get_config(test_nodes),

    %%%%
    %% Sleep to be sure that com is up and ready to be used.
    %%%%
    ComPid = rct_tlib:wait_for_com_to_start(identifier),
    ct:pal("ComPid: ~p .",[ComPid]),
    timer:sleep(10000),
    %%%%
    %% Get Beam Pid. Will be used when get memsize.
    %%%%
    BeamPid = get_correct_beam_pid(),
    ct:pal("BeamPid: ~p .",[BeamPid]),

    TestNodeBeamPid = get_testnode_beam_pid(),
    ct:pal("TestNodeBeamPid: ~p .",[TestNodeBeamPid]),

    TestOiPid = get_pid("test_oi"),
    ct:pal("TestOiPid: ~p .",[TestOiPid]),

    TestIftAppPid = get_pid("ift_app"),
    ct:pal("TestOiPid: ~p .",[TestIftAppPid]),

    TestAppPid = get_pid("test_app"),
    ct:pal("TestOiPid: ~p .",[TestAppPid]),


    %% %%%%
    %% %% Perform NC operations.
    %% %% Open, add, delete close nc.
    %% %%%%
    %% ct:pal("### Perform NC  operation first time before get memory size,", []),
    ok = perform_nc_operations_once(?NC_SessionNameList),

    %% %% %%%%
    %% %% %% get memory size
    %% %% %%%%
    [Start_Tot_bc,
     Start_Com_rss,
     Start_Beam_rss,
     Start_TestBeam_rss,
     Start_test_oi,
     Start_ift_app,
     Start_test_app] = get_used_mem_size_xxl(["tot_bc",
					      "com",
					      "beam",
					      "test_beam",
					      "test_oi",
					      "ift_app",
					      "test_app"], BeamPid, TestNodeBeamPid),

    %%%%
    %% For one hour.
    %% Perform NC and CLI operations.
    %% Open, add, delete close nc and cli.
    %%%%
    %% StartTime = erlang:timestamp(),
    StartTime = os:timestamp(),
    {End_Tot_bc,
     End_Com_rss,
     End_Beam_rss,
     End_TestBeam_rss,
     End_test_oi,
     End_ift_app,
     End_test_app,
     Nr} = loop_paralell_nc_operations_check_mem(StartTime,
						 Seconds,
						 %% 120,
						 Start_Tot_bc,
						 Start_Com_rss,
						 Start_Beam_rss,
						 Start_TestBeam_rss,
						 Start_test_oi,
						 Start_ift_app,
						 Start_test_app,
						 LogPath,
						 BeamPid,
						 TestNodeBeamPid),

    %%%%
    %% Write info to file
    %%%%
    FileName = "meas_check_memory_nc_cli_users_add_delete_rbsUnit_"++ integer_to_list(Hours) ++"_hour",
    rct_tlib:writeDataToFile(?LOG_DIR, FileName, "~p;~w;~p;~s;~s;~p;~s;~s;~p;~s;~s;~p;;~s;~s;~p;~s;~s;~p;~s;~s;~p;~s;~s;~p~w~n",
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
			      Start_TestBeam_rss,
			      End_TestBeam_rss,
			      list_to_integer(End_TestBeam_rss) - list_to_integer(Start_TestBeam_rss),
			      Start_test_oi,
			      End_test_oi,
			      list_to_integer(End_test_oi) - list_to_integer(Start_test_oi),
			      Start_ift_app,
			      End_test_oi,
			      list_to_integer(End_test_oi) - list_to_integer(Start_ift_app),
			      Start_test_app,
			      End_test_app,
			      list_to_integer(End_test_app) - list_to_integer(Start_test_app),
    			      NodeName
    			     ]),

    ct:pal("Start: com: ~s,  beam: ~s,  TestBeam: ~s,  test_oi: ~s,  ift_app: ~s,  test_app: ~s ~n", [Start_Com_rss,
												      Start_Beam_rss,
												      Start_TestBeam_rss,
												      Start_test_oi,
												      Start_ift_app,
												      Start_test_app
												     ]),

    ct:pal("End  : com: ~s,  beam: ~s,  TestBeam: ~s,  test_oi: ~s,  ift_app: ~s,  test_app: ~s ~n", [End_Com_rss,
												      End_Beam_rss,
												      End_TestBeam_rss,
												      End_test_oi,
												      End_ift_app,
												      End_test_app
												     ]),

    %%%%
    %% check nc sessions deleted
    %% check cli connection deleted
    %%%%
    ct:pal("Check nc session and cli connections is deleted.",[]),
    [] = rct_rpc:call(rpc, os, cmd, ["pgrep netconf"], 10000, noprint),

    ct:pal("All measured memory is loged at:",[]),
    ct:pal("LogPath: ~p, filename:  ~p ",[LogPath, ?All_NC_CLI_OPERATION_MEASURED_FILE]),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    ok.



%%--------------------------------------------------------------------
%% @doc
%% calls do_meas_check_memory_alarm with numbers of Hours that will perform the actual test.
%% @spec meas_check_memory_alarm_generate_cease_1_hour(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
meas_check_memory_alarm_generate_cease_1_hour(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_check_memory_alarm(1, LogPath).

meas_check_memory_alarm_generate_cease_10_hour(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_check_memory_alarm(10, LogPath).

%%--------------------------------------------------------------------
%% @doc
%% Check memory size after several generate / cease alarms. <br/>
%% This TC log measured memory size to file.<br/>
%% - reboot node. <br/>
%% - Open cli connections and config snmp. <br/>
%% - Check used memory for com, beam. This will be the starting point!<br/>
%%   ps -eo fname,vsz,rss | egrep '(beam)|(com)' <br/>
%% - For one hour. Do Generate  and cease alarms. <br/>
%% - Check used memory for com, beam. This will be the end point!<br/>
%%   ps -eo fname,vsz,rss | egrep '(beam)|(com)' <br/>
%%   ps -eo fname,rss | egrep '(snmpd)' <br/>
%% - Close cli connections and delete config snmp. <br/>e
%% @spec do_meas_check_memory_alarm(Hours, LogPath) -> ok
%% @end
%%--------------------------------------------------------------------
do_meas_check_memory_alarm(Hours, LogPath) ->
    ct:pal("### Check memory, several cli users config snmp, generate / cease Alarm. ~p hours!", [Hours]),
    [{_, NodeName}] = ct:get_config(test_nodes),
    %%%%
    %% Get sw version, using cli
    %%%%
    [H|_] = ?CLI_SessionNameList,
    CXS_label = get_sw_version(H),
    Seconds = Hours*60*60,

    %%%%
    %% reboot
    %%%%
    ct:pal("### reboot!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ok = rct_rs232:login(console),
    net_kernel:disconnect(ErlNode),
    ok = ct_telnet:send(console, "reboot"),
    {ok,_} = ct_telnet:expect(console, "du1 login", [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Sleep to be sure that com is up and ready to be used.
    %%%%
    ComPid = rct_tlib:wait_for_com_to_start(identifier),
    ct:pal("ComPid: ~p .",[ComPid]),
    timer:sleep(10000), %% Be sure that not healthcheck does not restart com.
    %%%%
    %% Get Beam Pid.
    %%%%
    BeamPid = get_correct_beam_pid(),
    ct:pal("BeamPid: ~p .",[BeamPid]),

    %%%%
    %% create cli connection.
    %%%%
    ct:pal("### Open nc sessions and cli connections.",[]),
    cli_create_connection(?CLI_SessionNameList),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    %% test_server:break("break"),
    SnmpdPidA = string:tokens(rct_rpc:call(rpc, os, cmd, ["pgrep snmpd"], 10000, noprint), "\n "),
    %%%%
    %% cli conf snmp.
    %%%%
    ct:pal("### Create conf snmp.",[]),
    cli_create_conf_snmp(),
    timer:sleep(5000), % wait for opstate to be enable.
    cli_check_conf_snmp_created(),
    timer:sleep(5000), %% wait!, Pid will be changed at startup!
    SnmpdPidB = string:tokens(rct_rpc:call(rpc, os, cmd, ["pgrep snmpd"], 10000, noprint), "\n "),
    case SnmpdPidA of
	[] ->
	    SnmpdPid = SnmpdPidB;
	_ ->
	    SnmpdPid = lists:delete(lists:flatten(SnmpdPidA), SnmpdPidB)
    end,
    %% ct:pal("A: ~p",[SnmpdPidA]),
    %% ct:pal("B: ~p",[SnmpdPidB]),
    %% ct:pal("C: ~p",[SnmpdPid]),


    %%%%
    %%get start memory size
    %%%%
    [Start_Com_rss, Start_Beam_rss, Start_Snmpd_rss] = get_used_mem_size([com, beam, snmpd], BeamPid, SnmpdPid),

    %%%%
    %% For one hour.
    %% Generate / Cease Alarm.
    %%%%
    StartTime = erlang:timestamp(),
    {End_Com_rss, End_Beam_rss, End_Snmpd_rss, Nr} = loop_gen_cease_alarm_check_mem(StartTime,
										    Seconds,
										    Start_Com_rss,
										    Start_Beam_rss,
										    Start_Snmpd_rss,
										    LogPath,
										    BeamPid,
										    SnmpdPid),

    FileName = "meas_check_memory_alarm_generate_cease_"++ integer_to_list(Hours) ++"_hour",
    %% rct_tlib:writeDataToFile(?LOG_DIR, ?ALARM_MEASURED_FILE, "~p;~w;~p;~s;~s;~p;~s;~s;~p;~s;~s;~p;~w ~n",
    rct_tlib:writeDataToFile(?LOG_DIR, FileName, "~p;~w;~p;~s;~s;~p;~s;~s;~p;~s;~s;~p;~w ~n",
    			     [httpd_util:rfc1123_date(),
			      CXS_label,
    			      Nr,
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


    ct:pal("Start: com: ~s,  beam: ~s,  snmpd: ~s ~n", [Start_Com_rss,
							Start_Beam_rss,
							Start_Snmpd_rss
						       ]),

    ct:pal("End  : com: ~s,  beam: ~s,  snmpd: ~s ~n", [End_Com_rss,
							End_Beam_rss,
							End_Snmpd_rss
						       ]),

    %%%%
    %% check that snmpd has not restarted.
    %%%%
    SnmpdPid2 = string:tokens(rct_rpc:call(rpc, os, cmd, ["pgrep snmpd"], 10000, noprint), "\n "),
    %% ct:pal("Start_SnmpdPid: ~p",[SnmpdPid]),
    %% ct:pal("End_SnmpdPid: ~p",[SnmpdPid2]),
    SnmpdPidB=SnmpdPid2,

    %%%%
    %% cli delete conf snmp.
    %%%%
    ct:pal("### Delete conf snmp.",[]),
    cli_delete_conf_snmp(),
    cli_check_conf_snmp_deleted(),

    %%%%
    %% Close cli connections.
    %%%%
    ct:pal("Close nc sessions and cli connections.",[]),
    cli_remove_connection(?CLI_SessionNameList, noprint),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),


    %%%%
    %% check cli connection deleted
    %%%%
    ct:pal("Check nc session and cli connections is deleted.",[]),
    [] = rct_rpc:call(rpc, os, cmd, ["pgrep cli"], 10000, noprint),

    ct:pal("All measured memory is loged at:",[]),
    ct:pal("LogPath: ~p, filename:  ~p ",[LogPath, ?ALL_ALARM_MEASURED_FILE]),

    ok.


%%% Internal functions
%%--------------------------------------------------------------------
%% @doc
%% Loop nc and cli operations during a time intervall. <br/>
%% Chek memory size. <br/>
%% @end
%%--------------------------------------------------------------------
loop_nc_cli_operations_check_mem(StartTime, Duration, Start_Com_rss, Start_Beam_rss, LogPath, BeamPid) ->
    loop_nc_cli_operations_check_mem(StartTime, Duration, 1, Start_Com_rss, Start_Beam_rss, LogPath, BeamPid).

loop_nc_cli_operations_check_mem(StartTime, Duration, Nr, Start_Com_rss, Start_Beam_rss, LogPath, BeamPid) ->
    %% ct:pal("¤¤¤ Perform NC and CLI operation. Add / delete RBS units, check memory, nr: ~p", [Nr]),
    ok = perform_nc_cli_operations_once(noprint),

    %%%%
    %%get used memory size
    %%%%
    [End_Com_rss, End_Beam_rss] = get_used_mem_size([com, beam], BeamPid),

    case Nr rem 25 of
	0 ->
	    ct:pal("~p; Nr: ~p; Start_com: ~s; End_com: ~s; diff_com: ~p ; Start_beam: ~s; End_beam: ~s; diff_beam: ~p ~n",
    			     [httpd_util:rfc1123_date(),
    			      Nr,
    			      Start_Com_rss,
    			      End_Com_rss,
    			      list_to_integer(End_Com_rss) - list_to_integer(Start_Com_rss),
    			      Start_Beam_rss,
    			      End_Beam_rss,
    			      list_to_integer(End_Beam_rss) - list_to_integer(Start_Beam_rss)
    			     ]);
	_ ->
	    ok
    end,

    %% Logs will be stored in test log path.
    rct_tlib:writeDataToFile(LogPath, ?All_NC_CLI_OPERATION_MEASURED_FILE, "~p; Nr: ~p; Start_com: ~s; End_com: ~s; diff_com: ~p ; Start_beam: ~s; End_beam: ~s; diff_beam: ~p ~n",
    			     [httpd_util:rfc1123_date(),
    			      Nr,
    			      Start_Com_rss,
    			      End_Com_rss,
    			      list_to_integer(End_Com_rss) - list_to_integer(Start_Com_rss),
    			      Start_Beam_rss,
    			      End_Beam_rss,
    			      list_to_integer(End_Beam_rss) - list_to_integer(Start_Beam_rss)
    			     ]),

    evaluate_mem_size(Start_Com_rss, End_Com_rss, Start_Beam_rss, End_Beam_rss),

    CheckTime = erlang:timestamp(),
    TimeDiff = trunc(timer:now_diff(CheckTime, StartTime) / 1000 / 1000),
    %% ct:pal("TimeDiff: ~p", [TimeDiff]),
    case TimeDiff > Duration of
	true ->
	    {End_Com_rss, End_Beam_rss, Nr};
	false ->
	   loop_nc_cli_operations_check_mem(StartTime, Duration, Nr+1, Start_Com_rss, Start_Beam_rss, LogPath, BeamPid)
    end.


%%%%%%%%%%%%%%%
%% XXL
%%%%%%%%%%%%%%%
loop_nc_operations_check_mem(StartTime, Duration, Start_Tot_bc, Start_Com_rss, Start_Beam_rss, Start_TestBeam_rss, Start_test_oi, Start_ift_app, Start_test_app, LogPath, BeamPid, TestNodeBeamPid, CpuLimitPids) ->
    loop_nc_operations_check_mem(StartTime, Duration, 0, Start_Tot_bc, Start_Com_rss, Start_Beam_rss,Start_TestBeam_rss, Start_test_oi, Start_ift_app, Start_test_app,  LogPath, BeamPid, TestNodeBeamPid, CpuLimitPids).

loop_nc_operations_check_mem(StartTime, Duration, Nr, Start_Tot_bc, Start_Com_rss, Start_Beam_rss, Start_TestBeam_rss, Start_test_oi, Start_ift_app, Start_test_app, LogPath, BeamPid, TestNodeBeamPid, CpuLimitPids) ->
    %% ct:pal("¤¤¤ Perform NC operation. Add / delete RBS units, check memory, nr: ~p", [Nr]),
    ok = perform_nc_operations_once(?NC_SessionNameList),

    %%%%
    %%get used memory size
    %%%%
    [End_Tot_bc,
     End_Com_rss,
     End_Beam_rss,
     End_TestBeam_rss,
     End_test_oi,
     End_ift_app,
     End_test_app] = get_used_mem_size_xxl(["tot_bc",
    					    "com",
    					    "beam",
    					    "test_beam",
    					    "test_oi",
    					    "ift_app",
    					    "test_app"], BeamPid, TestNodeBeamPid),

    case Nr rem 1000 of
	0 ->
	    Top = rct_rpc:call(rpc, os, cmd, ["top -b -n 1"], 10000, noprint),
	    TOP = string:tokens(Top, "\n"),
	    ct:log("Nr: ~p, ~p",[Nr, TOP]),

	    CpuLimitMemUsage = get_pid_mem_usage(CpuLimitPids, "cpulimit"),
	    ct:log("CpuLimitMemUsage: ~p .",[CpuLimitMemUsage]);
	_ ->
	    ok
    end,

    case Nr rem 50 of
	0 ->
 	    GetNrOfProc = rct_rpc:call(rpc, os, cmd, ["ps -A | wc"], 10000, noprint),
	    [NrOfProc | _T] = string:tokens(GetNrOfProc, " \n"),

	    B = rct_rpc:call(rpc, ets, all, [], 10000, noprint),
	    BB = length(B),
	    ct:log("Nr of ets element: ~p",[BB]),

	    TimeStamp = os:timestamp(),
	    PassedTime = trunc(timer:now_diff(TimeStamp, StartTime) / 1000 / 1000),

	    %% Needed for Jockes plot.
	    ct:pal("~p; Nr: ~p; Time: ~p; Start_tot_bc: ~s; End_tot_bc: ~s; diff_tot_bc: ~p ; Start_com: ~s; End_com: ~s; diff_com: ~p ; Start_beam: ~s; End_beam: ~s; diff_beam: ~p ~n",
		   [httpd_util:rfc1123_date(),
		    Nr,
		    PassedTime,
		    Start_Tot_bc,
		    End_Tot_bc,
		    list_to_integer(End_Tot_bc) - list_to_integer(Start_Tot_bc),
		    Start_Com_rss,
		    End_Com_rss,
		    list_to_integer(End_Com_rss) - list_to_integer(Start_Com_rss),
		    Start_Beam_rss,
		    End_Beam_rss,
		    list_to_integer(End_Beam_rss) - list_to_integer(Start_Beam_rss)
		   ]),

	    ct:log("~p; Nr: ~p; Time: ~p;  NoOfProc: ~p;~n"
	    	   "Start_tot_bc: ~s; End_tot_bc: ~s; diff_tot_bc: ~p ; ~n"
	    	   "Start_com: ~s; End_com: ~s; diff_com: ~p ; ~n"
	    	   "Start_beam: ~s; End_beam: ~s; diff_beam: ~p ; ~n"
	    	   "Start_TestBeam: ~s; End_TestBeam: ~s; diff_TestBeam: ~p ; ~n"
	    	   "Start_test_oi: ~s; End_test_oi: ~s; diff_test_oi: ~p ; ~n"
	    	   "Start_ift_app: ~s; End_ift_app: ~s; diff_ift_app: ~p ; ~n"
	    	   "Start_test_app: ~s; End_test_app: ~s; diff_test_app: ~p~n",
    	    		     [httpd_util:rfc1123_date(),
    	    		      Nr,
	    		      PassedTime,
			      NrOfProc,
	    		      Start_Tot_bc,
	    		      End_Tot_bc,
	    		      list_to_integer(End_Tot_bc) - list_to_integer(Start_Tot_bc),
    	    		      Start_Com_rss,
    	    		      End_Com_rss,
    	    		      list_to_integer(End_Com_rss) - list_to_integer(Start_Com_rss),
    	    		      Start_Beam_rss,
    	    		      End_Beam_rss,
    	    		      list_to_integer(End_Beam_rss) - list_to_integer(Start_Beam_rss),
	    		      Start_TestBeam_rss,
	    		      End_TestBeam_rss,
	    		      list_to_integer(End_TestBeam_rss) - list_to_integer(Start_TestBeam_rss),
	    		      Start_test_oi,
	    		      End_test_oi,
	    		      list_to_integer(End_test_oi) - list_to_integer(Start_test_oi),
	    		      Start_ift_app,
	    		      End_ift_app,
	    		      list_to_integer(End_ift_app) - list_to_integer(Start_ift_app),
	    		      Start_test_app,
	    		      End_test_app,
	    		      list_to_integer(End_test_app) - list_to_integer(Start_test_app)
	    		     ]),

    %% Logs will be stored in test log path.
    rct_tlib:writeDataToFile(LogPath, ?All_NC_CLI_OPERATION_MEASURED_FILE, "~p; Nr: ~p; Time: ~p;"
			     "Start_tot_bc: ~s; End_tot_bc: ~s; diff_tot_bc: ~p ; "
			     "Start_com: ~s; End_com: ~s; diff_com: ~p ; "
			     "Start_beam: ~s; End_beam: ~s; diff_beam: ~p ~n",
    			     [httpd_util:rfc1123_date(),
    			      Nr,
			      PassedTime,
			      Start_Tot_bc,
			      End_Tot_bc,
			      list_to_integer(End_Tot_bc) - list_to_integer(Start_Tot_bc),
    			      Start_Com_rss,
    			      End_Com_rss,
    			      list_to_integer(End_Com_rss) - list_to_integer(Start_Com_rss),
    			      Start_Beam_rss,
    			      End_Beam_rss,
    			      list_to_integer(End_Beam_rss) - list_to_integer(Start_Beam_rss)
    			     ]),

    rct_tlib:writeDataToFile(LogPath, ?All_NC_CLI_OPERATION_MEASURED_FILE_2, "~p; Nr: ~p; Time: ~p;"
			     "Start_TestBeam: ~s; End_TestBeam: ~s; diff_TestBeam: ~p ; "
			     "Start_test_oi: ~s; End_test_oi: ~s; diff_test_oi: ~p ; "
			     "Start_ift_app: ~s; End_ift_app: ~s; diff_ift_app: ~p ; "
			     "Start_test_app: ~s; End_test_app: ~s; diff_test_app: ~p ~n",
    			     [httpd_util:rfc1123_date(),
    			      Nr,
			      PassedTime,
			      Start_TestBeam_rss,
			      End_TestBeam_rss,
			      list_to_integer(End_TestBeam_rss) - list_to_integer(Start_TestBeam_rss),
			      Start_test_oi,
			      End_test_oi,
			      list_to_integer(End_test_oi) - list_to_integer(Start_test_oi),
			      Start_ift_app,
			      End_ift_app,
			      list_to_integer(End_ift_app) - list_to_integer(Start_ift_app),
			      Start_test_app,
			      End_test_app,
			      list_to_integer(End_test_app) - list_to_integer(Start_test_app)
    			     ]);
	_ ->
	    ok
    end,

    CheckTime = os:timestamp(),
    TimeDiff = trunc(timer:now_diff(CheckTime, StartTime) / 1000 / 1000),
    %% ct:pal("TimeDiff: ~p", [TimeDiff]),
    case TimeDiff > Duration of
	true ->
	    %% {End_Tot_bc, End_Com_rss, End_Beam_rss, Nr};
	    {End_Tot_bc,
	     End_Com_rss,
	     End_Beam_rss,
	     End_TestBeam_rss,
	     End_test_oi,
	     End_ift_app,
	     End_test_app,
	     Nr};
	false ->
	   %% loop_nc_operations_check_mem(StartTime, Duration, Nr+1, Start_Tot_bc, Start_Com_rss, Start_Beam_rss, LogPath, BeamPid)
	    loop_nc_operations_check_mem(StartTime, Duration, Nr+1, Start_Tot_bc, Start_Com_rss, Start_Beam_rss, Start_TestBeam_rss, Start_test_oi, Start_ift_app, Start_test_app, LogPath, BeamPid, TestNodeBeamPid, CpuLimitPids)
    end.

%%%%%%%%%%%%%%%
%% XXL TCU
%%%%%%%%%%%%%%%
loop_nc_operations_check_mem_tcu(StartTime, Duration, Start_Tot_bc, Start_Com_rss, Start_Beam_rss, Start_TestBeam_rss, Start_test_oi, Start_ift_app, Start_test_app, LogPath, BeamPid, TestNodeBeamPid) ->
    loop_nc_operations_check_mem_tcu(StartTime, Duration, 0, Start_Tot_bc, Start_Com_rss, Start_Beam_rss,Start_TestBeam_rss, Start_test_oi, Start_ift_app, Start_test_app,  LogPath, BeamPid, TestNodeBeamPid).

loop_nc_operations_check_mem_tcu(StartTime, Duration, Nr, Start_Tot_bc, Start_Com_rss, Start_Beam_rss, Start_TestBeam_rss, Start_test_oi, Start_ift_app, Start_test_app, LogPath, BeamPid, TestNodeBeamPid) ->
    %% ct:pal("¤¤¤ Perform NC operation. Add / delete RBS units, check memory, nr: ~p", [Nr]),
    ok = perform_nc_operations_once(?NC_SessionNameList),

    %%%%
    %%get used memory size
    %%%%
    [End_Tot_bc,
     End_Com_rss,
     End_Beam_rss,
     End_TestBeam_rss,
     End_test_oi,
     End_ift_app,
     End_test_app] = get_used_mem_size_xxl(["tot_bc",
    					    "com",
    					    "beam",
    					    "test_beam",
    					    "test_oi",
    					    "ift_app",
    					    "test_app"], BeamPid, TestNodeBeamPid),

    case Nr rem 1000 of
	0 ->
	    Top = rct_rpc:call(rpc, os, cmd, ["top -b -n 1"], 10000, noprint),
	    TOP = string:tokens(Top, "\n"),
	    ct:log("Nr: ~p, ~p",[Nr, TOP]);
	_ ->
	    ok
    end,

    case Nr rem 50 of
	0 ->
 	    GetNrOfProc = rct_rpc:call(rpc, os, cmd, ["ps -A | wc"], 10000, noprint),
	    [NrOfProc | _T] = string:tokens(GetNrOfProc, " \n"),

	    B = rct_rpc:call(rpc, ets, all, [], 10000, noprint),
	    BB = length(B),
	    ct:log("Nr of ets element: ~p",[BB]),

	    TimeStamp = os:timestamp(),
	    PassedTime = trunc(timer:now_diff(TimeStamp, StartTime) / 1000 / 1000),

	    %% Needed for Jockes plot.
	    ct:pal("~p; Nr: ~p; Time: ~p; Start_tot_bc: ~s; End_tot_bc: ~s; diff_tot_bc: ~p ; Start_com: ~s; End_com: ~s; diff_com: ~p ; Start_beam: ~s; End_beam: ~s; diff_beam: ~p ~n",
		   [httpd_util:rfc1123_date(),
		    Nr,
		    PassedTime,
		    Start_Tot_bc,
		    End_Tot_bc,
		    list_to_integer(End_Tot_bc) - list_to_integer(Start_Tot_bc),
		    Start_Com_rss,
		    End_Com_rss,
		    list_to_integer(End_Com_rss) - list_to_integer(Start_Com_rss),
		    Start_Beam_rss,
		    End_Beam_rss,
		    list_to_integer(End_Beam_rss) - list_to_integer(Start_Beam_rss)
		   ]),

	    ct:log("~p; Nr: ~p; Time: ~p;  NoOfProc: ~p;~n"
	    	   "Start_tot_bc: ~s; End_tot_bc: ~s; diff_tot_bc: ~p ; ~n"
	    	   "Start_com: ~s; End_com: ~s; diff_com: ~p ; ~n"
	    	   "Start_beam: ~s; End_beam: ~s; diff_beam: ~p ; ~n"
	    	   "Start_TestBeam: ~s; End_TestBeam: ~s; diff_TestBeam: ~p ; ~n"
	    	   "Start_test_oi: ~s; End_test_oi: ~s; diff_test_oi: ~p ; ~n"
	    	   "Start_ift_app: ~s; End_ift_app: ~s; diff_ift_app: ~p ; ~n"
	    	   "Start_test_app: ~s; End_test_app: ~s; diff_test_app: ~p~n",
    	    		     [httpd_util:rfc1123_date(),
    	    		      Nr,
	    		      PassedTime,
			      NrOfProc,
	    		      Start_Tot_bc,
	    		      End_Tot_bc,
	    		      list_to_integer(End_Tot_bc) - list_to_integer(Start_Tot_bc),
    	    		      Start_Com_rss,
    	    		      End_Com_rss,
    	    		      list_to_integer(End_Com_rss) - list_to_integer(Start_Com_rss),
    	    		      Start_Beam_rss,
    	    		      End_Beam_rss,
    	    		      list_to_integer(End_Beam_rss) - list_to_integer(Start_Beam_rss),
	    		      Start_TestBeam_rss,
	    		      End_TestBeam_rss,
	    		      list_to_integer(End_TestBeam_rss) - list_to_integer(Start_TestBeam_rss),
	    		      Start_test_oi,
	    		      End_test_oi,
	    		      list_to_integer(End_test_oi) - list_to_integer(Start_test_oi),
	    		      Start_ift_app,
	    		      End_ift_app,
	    		      list_to_integer(End_ift_app) - list_to_integer(Start_ift_app),
	    		      Start_test_app,
	    		      End_test_app,
	    		      list_to_integer(End_test_app) - list_to_integer(Start_test_app)
	    		     ]),

    %% Logs will be stored in test log path.
    rct_tlib:writeDataToFile(LogPath, ?All_NC_CLI_OPERATION_MEASURED_FILE, "~p; Nr: ~p; Time: ~p;"
			     "Start_tot_bc: ~s; End_tot_bc: ~s; diff_tot_bc: ~p ; "
			     "Start_com: ~s; End_com: ~s; diff_com: ~p ; "
			     "Start_beam: ~s; End_beam: ~s; diff_beam: ~p ~n",
    			     [httpd_util:rfc1123_date(),
    			      Nr,
			      PassedTime,
			      Start_Tot_bc,
			      End_Tot_bc,
			      list_to_integer(End_Tot_bc) - list_to_integer(Start_Tot_bc),
    			      Start_Com_rss,
    			      End_Com_rss,
    			      list_to_integer(End_Com_rss) - list_to_integer(Start_Com_rss),
    			      Start_Beam_rss,
    			      End_Beam_rss,
    			      list_to_integer(End_Beam_rss) - list_to_integer(Start_Beam_rss)
    			     ]),

    rct_tlib:writeDataToFile(LogPath, ?All_NC_CLI_OPERATION_MEASURED_FILE_2, "~p; Nr: ~p; Time: ~p;"
			     "Start_TestBeam: ~s; End_TestBeam: ~s; diff_TestBeam: ~p ; "
			     "Start_test_oi: ~s; End_test_oi: ~s; diff_test_oi: ~p ; "
			     "Start_ift_app: ~s; End_ift_app: ~s; diff_ift_app: ~p ; "
			     "Start_test_app: ~s; End_test_app: ~s; diff_test_app: ~p ~n",
    			     [httpd_util:rfc1123_date(),
    			      Nr,
			      PassedTime,
			      Start_TestBeam_rss,
			      End_TestBeam_rss,
			      list_to_integer(End_TestBeam_rss) - list_to_integer(Start_TestBeam_rss),
			      Start_test_oi,
			      End_test_oi,
			      list_to_integer(End_test_oi) - list_to_integer(Start_test_oi),
			      Start_ift_app,
			      End_ift_app,
			      list_to_integer(End_ift_app) - list_to_integer(Start_ift_app),
			      Start_test_app,
			      End_test_app,
			      list_to_integer(End_test_app) - list_to_integer(Start_test_app)
    			     ]);
	_ ->
	    ok
    end,

    CheckTime = os:timestamp(),
    TimeDiff = trunc(timer:now_diff(CheckTime, StartTime) / 1000 / 1000),
    %% ct:pal("TimeDiff: ~p", [TimeDiff]),
    case TimeDiff > Duration of
	true ->
	    %% {End_Tot_bc, End_Com_rss, End_Beam_rss, Nr};
	    {End_Tot_bc,
	     End_Com_rss,
	     End_Beam_rss,
	     End_TestBeam_rss,
	     End_test_oi,
	     End_ift_app,
	     End_test_app,
	     Nr};
	false ->
	    loop_nc_operations_check_mem_tcu(StartTime, Duration, Nr+1, Start_Tot_bc, Start_Com_rss, Start_Beam_rss, Start_TestBeam_rss, Start_test_oi, Start_ift_app, Start_test_app, LogPath, BeamPid, TestNodeBeamPid)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Loop alarm operations during a time intervall. <br/>
%% Chek memory size. <br/>
%% @end
%%--------------------------------------------------------------------
loop_gen_cease_alarm_check_mem(StartTime, Duration, Start_Com_rss, Start_Beam_rss, Start_Snmpd_rss, LogPath, BeamPid, Snmpd) ->
    loop_gen_cease_alarm_check_mem(StartTime, Duration, 1, Start_Com_rss, Start_Beam_rss, Start_Snmpd_rss, LogPath, BeamPid, Snmpd).

loop_gen_cease_alarm_check_mem(StartTime, Duration, Nr, Start_Com_rss, Start_Beam_rss, Start_Snmpd_rss, LogPath, BeamPid, Snmpd) ->
    %% ct:pal("¤¤¤ generate / cease alarm, nr: ~p", [Nr]),
    %% test_server:break("break"),

    %%%%
    %% Generate alarm.
    %%%%
    %% ct:pal("Generate alarms.",[]),
    ok = rct_rpc:call(rpc, lih_fm, request_LKF_fault_alarm, [], 10000, noprint),
    ok = rct_rpc:call(rpc, lih_fm, request_LKF_missing_alarm, [], 10000, noprint),
    ok = rct_rpc:call(rpc, lih_fm, request_emergency_unlock_alarm, [], 10000, noprint),
    ok = rct_rpc:call(rpc, lih_fm, request_emergency_unlock_expired_alarm, [], 10000, noprint),
    ok = rct_rpc:call(rpc, lih_fm, request_integration_unlock_alarm, [], 10000, noprint),
    ok = rct_rpc:call(rpc, lih_fm, request_integration_unlock_expired_alarm, [], 10000, noprint),
    ok = rct_rpc:call(rpc, lih_fm, request_production_unlock_alarm, [], 10000, noprint),
    ok = rct_rpc:call(rpc, lih_fm, request_production_unlock_expired_alarm, [], 10000, noprint),
    %%%%
    %% Check alarm is active.
    %%%%
    NeOfAlarms="8",
    cli_check_alarm_is_active(NeOfAlarms),

    %%%%
    %% Cease alarm.
    %%%%
    %% ct:pal("Cease alarms.",[]),
    ok = rct_rpc:call(rpc, lih_fm, cancel_LKF_fault_alarm, [], 10000, noprint),
    ok = rct_rpc:call(rpc, lih_fm, cancel_LKF_missing_alarm, [], 10000, noprint),
    ok = rct_rpc:call(rpc, lih_fm, cancel_emergency_unlock_alarm, [], 10000, noprint),
    ok = rct_rpc:call(rpc, lih_fm, cancel_emergency_unlock_expired_alarm, [], 10000, noprint),
    ok = rct_rpc:call(rpc, lih_fm, cancel_integration_unlock_alarm, [], 10000, noprint),
    ok = rct_rpc:call(rpc, lih_fm, cancel_integration_unlock_expired_alarm, [], 10000, noprint),
    ok = rct_rpc:call(rpc, lih_fm, cancel_production_unlock_alarm, [], 10000, noprint),
    ok = rct_rpc:call(rpc, lih_fm, cancel_production_unlock_expired_alarm, [], 10000, noprint),
    %%%%
    %% Check alarm is not active.
    %%%%
    cli_check_alarm_is_inactive(),

    %%%%
    %% Get mem size and evaluate.
    %%%%
    [End_Com_rss, End_Beam_rss, End_Snmpd_rss] = get_used_mem_size([com, beam, snmpd], BeamPid, Snmpd),

    %% Logs will be stored in test log path.
    rct_tlib:writeDataToFile(LogPath, ?ALL_ALARM_MEASURED_FILE, "~p; Nr: ~p; Start_com: ~s; End_com: ~s; diff_com: ~p ; Start_beam: ~s; End_beam: ~s; diff_beam: ~p ; Start_snmpd: ~s; End_snmpd: ~s; diff_snmpd: ~p ~n",
    			     [httpd_util:rfc1123_date(),
    			      Nr,
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

    %% ct:pal("### Start_snmpd_rss: ~p , End_snmpd_rss: ~p ",[Start_snmpd_rss, End_snmpd_rss]),
    evaluate_mem_size(Start_Com_rss, End_Com_rss, Start_Beam_rss, End_Beam_rss),
    evaluate_snmpd_mem_size(Start_Snmpd_rss, End_Snmpd_rss),

    CheckTime = erlang:timestamp(),
    TimeDiff = trunc(timer:now_diff(CheckTime, StartTime) / 1000 / 1000),
    %% ct:pal("TimeDiff: ~p", [TimeDiff]),
    case TimeDiff > Duration of
	true ->
	    {End_Com_rss, End_Beam_rss, End_Snmpd_rss, Nr};
	false ->
	   loop_gen_cease_alarm_check_mem(StartTime, Duration, Nr+1, Start_Com_rss, Start_Beam_rss, Start_Snmpd_rss, LogPath, BeamPid, Snmpd)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Perform nc and cli opearatiuons. <br/>
%% - Add rbsUnits. <br/>
%% - Delete rbsUnits. <br/>
%% PrintOpt  print | noprint   Used when wanted printouts in testlog. <br/>
%% @end
%%--------------------------------------------------------------------
perform_nc_cli_operations_once(PrintOpt) ->

    %%%%
    %% Add rbs uits
    %%%%
    %% ct:pal("Add rbsUnits.",[]),
    nc_add_rbsUnits(?NC_SessionNameList),
    cli_add_rbsUnits(?CLI_SessionNameList, PrintOpt),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    %%%%
    %% Check rbs units is added
    %%%%
    %% ct:pal("Check rbsUnits is created.",[]),
    ok = nc_check_rbs_units_is_added(?NC_SessionNameList),
    ok = check_all_rbs_units_is_added(?NC_SessionNameList, ?CLI_SessionNameList),

    %%%%
    %% Delete rbs units
    %%%%
    %% ct:pal("Delete rbsUnits.",[]),
    nc_delete_rbsUnits(?NC_SessionNameList),
    cli_delete_rbsUnits(?CLI_SessionNameList, PrintOpt),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    %%%%
    %% Check rbs units is deleted
    %%%%
    %% ct:pal("Check rbsUnits is deleted.",[]),
    ok = nc_check_rbs_units_is_deleted(?NC_SessionNameList),
    ok = cli_check_rbsUnits_deleted(?CLI_SessionNameList, PrintOpt),

    ok.

%%%%%%%%%%%%%%%%%
perform_nc_operations_once(NC_SessionNameList) ->
    %%%%
    %% Add rbs uits
    %%%%
    lists:foreach(fun(Name) ->

			  case ct_netconfc:open(Name, [{timeout, 300000}]) of
			      {ok,_} ->
				  ok;
			      Ret ->
				  ct:pal("Fail to open NC session: ~p ",[Ret])
			  end,

			  RbsUnitId = atom_to_list(Name),
			  UserLabel = "userLabel_" ++ RbsUnitId,
			  case ct_netconfc:edit_config(Name,running,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
									    [{managedElementId,[],["1"]},
									     {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
									      [{'equipmentId',[],["1"]},
									       {'RbsUnit',[{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
										[{rbsUnitId,[],[RbsUnitId]},
										 {userLabel,[],[UserLabel]} ]}]}]}, 300000) of
			      ok ->
				  case ct_netconfc:close_session(Name, 300000) of
				      ok ->
					  ok;
				      Res ->
					  ct:pal("Fail to close session: ~p ",[Res])
				  end;
			      Res1  ->
				  ct:pal("Fail to add RbsUnit: ~p ",[Res1])
			  end
		  end, NC_SessionNameList),

    case rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint) of
	[] ->
	    ok;
	Result1 ->
	    ct:pal("Fail to send sync via rpc: ~p.\n Check top ",[Result1]),
	    rct_rs232:login(console),
	    ct_telnet:cmd(console, "top -b -n 1", 30000)
    end,

    %%%%
    %% Delete rbs units
    %%%%
    lists:foreach(fun(SesionName) ->
			  case ct_netconfc:open(SesionName, [{timeout, 300000}]) of
			      {ok,_} ->
				  ok;
			      Ret ->
				  ct:pal("Fail to open NC session: ~p ",[Ret])
			  end,

			  RbsUnitId =atom_to_list(SesionName),
			  UserLabel = "userLabel_" ++ RbsUnitId,
			  %% ct:pal("### Delete RBS Unit id: ~p, userlabel: ~p", [RbsUnitId, UserLabel]),
			  case ct_netconfc:edit_config(SesionName,running,{'ManagedElement',
								       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
								       [{managedElementId,[],["1"]},
									{'Equipment',
									 [{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
									      [{'equipmentId',[],["1"]},
									       {'RbsUnit',
										[{xmlns,"urn:com:ericsson:ecim:rbsunit"},
										 {'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
										 {'nc:operation',"delete"}],
										[{rbsUnitId,[],[RbsUnitId]},
										 {userLabel,[],[UserLabel]}]}]}]}, 300000) of
			      ok ->
				  case ct_netconfc:close_session(SesionName, 300000) of
				      ok ->
					  ok;
				      Res2 ->
					  ct:pal("Fail to close session: ~p ",[Res2])
				  end;
			      Res3 ->
				  ct:pal("Fail to delete RbsUnit: ~p ",[Res3])
			  end
		  end, NC_SessionNameList),

    case rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint) of
	[] ->
	    ok;
	Result2 ->
	    ct:pal("Fail to send sync via rpc: ~p.\n Check top ",[Result2]),
	    rct_rs232:login(console),
	    ct_telnet:cmd(console, "top -b -n 1", 30000)
    end,

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%
%% paralell nc operation, xxl
loop_paralell_nc_operations_check_mem(StartTime, Duration, Start_Tot_bc, Start_Com_rss, Start_Beam_rss, Start_TestBeam_rss, Start_test_oi, Start_ift_app, Start_test_app, LogPath, BeamPid, TestNodeBeamPid) ->
    loop_paralell_nc_operations_check_mem(StartTime, Duration, 0, Start_Tot_bc, Start_Com_rss, Start_Beam_rss,Start_TestBeam_rss, Start_test_oi, Start_ift_app, Start_test_app,  LogPath, BeamPid, TestNodeBeamPid).

loop_paralell_nc_operations_check_mem(StartTime, Duration, Nr, Start_Tot_bc, Start_Com_rss, Start_Beam_rss, Start_TestBeam_rss, Start_test_oi, Start_ift_app, Start_test_app, LogPath, BeamPid, TestNodeBeamPid) ->
    ct:pal("¤¤¤ Perform paralell NC operation. Add / delete RBS units, check memory, nr: ~p", [Nr]),

    ok = perform_paralell_nc_operations_once(?NC_SessionNameList),

    %%%%
    %%get used memory size
    %%%%
    %% [End_Tot_bc,End_Com_rss, End_Beam_rss] = get_used_mem_size([tot_bc, com, beam], BeamPid),
    [End_Tot_bc,
     End_Com_rss,
     End_Beam_rss,
     End_TestBeam_rss,
     End_test_oi,
     End_ift_app,
     End_test_app] = get_used_mem_size_xxl(["tot_bc",
					    "com",
					    "beam",
					    "test_beam",
					    "test_oi",
					    "ift_app",
					    "test_app"], BeamPid, TestNodeBeamPid),

    case Nr rem 1000 of
	0 ->
	    Top = rct_rpc:call(rpc, os, cmd, ["top -b -n 1"], 10000, noprint),
	    TOP = string:tokens(Top, "\n"),
	    ct:pal("Nr: ~p, ~p",[Nr, TOP]);
	_ ->
	    ok
    end,

    case Nr rem 50 of
	0 ->
	    TimeStamp = os:timestamp(),
	    PassedTime = trunc(timer:now_diff(TimeStamp, StartTime) / 1000 / 1000),

	    ct:pal("~p; Nr: ~p; Time: ~p; Start_tot_bc: ~s; End_tot_bc: ~s; diff_tot_bc: ~p ; Start_com: ~s; End_com: ~s; diff_com: ~p ; Start_beam: ~s; End_beam: ~s; diff_beam: ~p ; Start_TestBeam: ~s; End_TestBeam: ~s; diff_TestBeam: ~p ; Start_test_oim: ~s; End_test_oi: ~s; diff_test_oi: ~p ; Start_ift_app: ~s; End_ift_app: ~s; diff_ift_app: ~p ; Start_test_app: ~s; End_test_app: ~s; diff_test_app: ~p~n",
    			     [httpd_util:rfc1123_date(),
    			      Nr,
			      PassedTime,
			      Start_Tot_bc,
			      End_Tot_bc,
			      list_to_integer(End_Tot_bc) - list_to_integer(Start_Tot_bc),
    			      Start_Com_rss,
    			      End_Com_rss,
    			      list_to_integer(End_Com_rss) - list_to_integer(Start_Com_rss),
    			      Start_Beam_rss,
    			      End_Beam_rss,
    			      list_to_integer(End_Beam_rss) - list_to_integer(Start_Beam_rss),
			      Start_TestBeam_rss,
			      End_TestBeam_rss,
			      list_to_integer(End_TestBeam_rss) - list_to_integer(Start_TestBeam_rss),
			      Start_test_oi,
			      End_test_oi,
			      list_to_integer(End_test_oi) - list_to_integer(Start_test_oi),
			      Start_ift_app,
			      End_test_oi,
			      list_to_integer(End_test_oi) - list_to_integer(Start_ift_app),
			      Start_test_app,
			      End_test_app,
			      list_to_integer(End_test_app) - list_to_integer(Start_test_app)
			     ]);
	_ ->
	    ok
    end,

    %% Logs will be stored in test log path.
    rct_tlib:writeDataToFile(LogPath, ?All_MEASURED_LOGS, "~p; Nr: ~p; Start_tot_bc: ~s; End_tot_bc: ~s; diff_tot_bc: ~p ; Start_com: ~s; End_com: ~s; diff_com: ~p ; Start_beam: ~s; End_beam: ~s; diff_beam: ~p ; Start_TestBeam: ~s; End_TestBeam: ~s; diff_TestBeam: ~p ; Start_test_oim: ~s; End_test_oi: ~s; diff_test_oi: ~p ; Start_ift_app: ~s; End_ift_app: ~s; diff_ift_app: ~p ; Start_test_app: ~s; End_test_app: ~s; diff_test_app: ~p ~n",
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
			      Start_TestBeam_rss,
			      End_TestBeam_rss,
			      list_to_integer(End_TestBeam_rss) - list_to_integer(Start_TestBeam_rss),
			      Start_test_oi,
			      End_test_oi,
			      list_to_integer(End_test_oi) - list_to_integer(Start_test_oi),
			      Start_ift_app,
			      End_test_oi,
			      list_to_integer(End_test_oi) - list_to_integer(Start_ift_app),
			      Start_test_app,
			      End_test_app,
			      list_to_integer(End_test_app) - list_to_integer(Start_test_app)
    			     ]),

    %% evaluate_mem_size(Start_Com_rss, End_Com_rss, Start_Beam_rss, End_Beam_rss),

    CheckTime = os:timestamp(),
    TimeDiff = trunc(timer:now_diff(CheckTime, StartTime) / 1000 / 1000),
    %% ct:pal("TimeDiff: ~p", [TimeDiff]),
    case TimeDiff > Duration of
	true ->
	    {End_Tot_bc,
	     End_Com_rss,
	     End_Beam_rss,
	     End_TestBeam_rss,
	     End_test_oi,
	     End_ift_app,
	     End_test_app,
	     Nr};
	false ->
	   loop_paralell_nc_operations_check_mem(StartTime, Duration, Nr+1, Start_Tot_bc, Start_Com_rss, Start_Beam_rss, Start_TestBeam_rss, Start_test_oi, Start_ift_app, Start_test_app, LogPath, BeamPid, TestNodeBeamPid)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
perform_paralell_nc_operations_once(NC_SessionNameList) ->
    Self = self(),

    %%%%
    %% Add rbs uits
    %%%%
    %% Self = self(),
    PidList_add = lists:map(fun(Name1) ->
				    spawn(fun() -> add_cmd(Name1),
						   Self ! add_done
					  end)
			    end,
			    NC_SessionNameList),

    %%%%
    %%Wait on answer from request.
    %%%%
    lists:foreach(fun(_PidA) ->
    			 receive
    			      add_done ->
    				 ok
    			 after 30000 ->
    				 ct:pal("Error no answer within time!", [])
    			 end
    		  end, PidList_add),

    case rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint) of
    	[] ->
    	    ok;
    	Result1 ->
    	    ct:pal("Fail to send sync via rpc: ~p.\n Check top ",[Result1]),
    	    rct_rs232:login(console),
    	    ct_telnet:cmd(console, "top -b -n 1", 30000)
    end,

    %%%%
    %% Delete rbs uits
    %%%%
    %% Self = self(),
    PidList_delete = lists:map(fun(Name2) ->
				    spawn(fun() -> delete_cmd(Name2),
						   Self ! delete_done
					  end)
			    end,
			    NC_SessionNameList),

    %%%%
    %%Wait on answer from request.
    %%%%
    lists:foreach(fun(_PidA) ->
    			 receive
    			      delete_done ->
    				 ok
    			 after 30000 ->
    				 ct:pal("Error no answer within time!", [])
    			 end
    		  end, PidList_delete),

    case rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint) of
    	[] ->
    	    ok;
    	Result2 ->
    	    ct:pal("Fail to send sync via rpc: ~p.\n Check top ",[Result2]),
    	    rct_rs232:login(console),
    	    ct_telnet:cmd(console, "top -b -n 1", 30000)
    end,

    NrOfRbsUnits2 = length(rct_rpc:call(rpc, ets, tab2list, [rbsUnit], 10000, noprint)),
    ct:pal("NrOfRbsUnits after delete: ~p", [NrOfRbsUnits2]),

    lists:foreach(fun(Name3) ->
			 ct_netconfc:close_session(Name3, 10000)
		  end, NC_SessionNameList),

    case rct_rpc:call(rpc, os, cmd, ["pgrep netconf"], 10000, noprint) of
	[] ->
	    ok;
	_ ->
	    rct_rpc:call(rpc, os, cmd, ["pkill netconf"], 10000, noprint)
    end,

    %% A = rct_rpc:call(rpc, supervisor, count_children, [sshd_sup], 2000, noprint),
    %% ct:pal("~p",[A]),
    B = rct_rpc:call(rpc, ets, all, [], 10000, noprint),
    ct:pal("~p",[B]),

    BB = length(B),
    ct:pal("~p",[BB]),

    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% perform_paralell_nc_operations_once(NC_SessionNameList) ->

%%     %%%%
%%     %% Add rbs uits
%%     %%%%
%%     Self = self(),
%%     Slave = fun(Name, Loop) ->
%% 		    receive
%% 			add_cmd ->
%% 			    add_cmd(Name),
%% 			    Self ! add_done;
%% 			delete_cmd ->
%% 			    delete_cmd(Name),
%% 			    Self ! delete_done;
%% 			exit_slave ->
%% 			    exit(normal)
%% 		    %% after 5000 ->
%% 		    %% %% 	    ct:pal("Exit slave due to timeout",[]),
%% 		    %% 	    exit(normal)
%% 		    end,
%% 		    Loop(Name, Loop)
%% 	    end,

%%     PidList = lists:map(fun(Name) ->
%% 				spawn(fun() ->
%% 					      Slave(Name, Slave)
%% 				      end)

%% 			end,
%% 			NC_SessionNameList),

%%     %%%%
%%     %% Send Add rbsUnit paralell request
%%     %%%%
%%     lists:foreach(fun(Pid1) ->
%% 			  Pid1 ! add_cmd
%% 		  end, PidList),


%%     %%%%
%%     %% Wait on answer from add request.
%%     %%%%
%%     lists:foreach(fun(_Pid_1) ->
%% 			  receive
%%     			      add_done ->
%% 				  ok
%% 			  after 1000 ->
%% 				  ct:pal("Error no answer from add, within time!", [])
%% 			  end
%% 		  end, PidList),

%%     case rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint) of
%%     	[] ->
%%     	    ok;
%%     	Result1 ->
%%     	    ct:pal("Fail to send sync via rpc: ~p.\n Check top ",[Result1]),
%%     	    rct_rs232:login(console),
%%     	    ct_telnet:cmd(console, "top -b -n 1", 30000)
%%     end,

%%     NrOfRbsUnits = length(rct_rpc:call(rpc, ets, tab2list, [rbsUnit], 500, noprint)),
%%     ct:pal("NrOfRbsUnits after add: ~p", [NrOfRbsUnits]),

%%     %%%%
%%     %% Send Delete rbs units paralell request
%%     %%%%
%%     lists:foreach(fun(Pid2) ->
%% 			  Pid2 ! delete_cmd
%% 		  end, PidList),

%%     %%%%
%%     %% Wait on answer from the Delete request.
%%     %%%%
%%     lists:foreach(fun(_Pid_3) ->
%% 			  receive
%% 			      delete_done ->
%% 				  ok
%% 			  after 1000 ->
%% 				 ct:pal("Error no answer from delete, within time!", [])
%% 			 end
%% 		  end, PidList),


%%     case rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint) of
%%     	[] ->
%%     	    ok;
%%     	Result2 ->
%%     	    ct:pal("Fail to send sync via rpc: ~p.\n Check top ",[Result2]),
%%     	    rct_rs232:login(console),
%%     	    ct_telnet:cmd(console, "top -b -n 1", 30000)
%%     end,

%%     NrOfRbsUnits2 = length(rct_rpc:call(rpc, ets, tab2list, [rbsUnit], 500, noprint)),
%%     ct:pal("NrOfRbsUnits after delete: ~p", [NrOfRbsUnits2]),

%%     lists:foreach(fun(Pid5) ->
%%     			  Pid5 ! exit_slave
%%     		  end, PidList),

%%     lists:foreach(fun(SessionName) ->
%% 			 ct_netconfc:close_session(SessionName, 3000)
%% 		  end, NC_SessionNameList),

%%     case rct_rpc:call(rpc, os, cmd, ["pgrep netconf"], 5000, noprint) of
%% 	[] ->
%% 	    ok;
%% 	_ ->
%% 	    rct_rpc:call(rpc, os, cmd, ["pkill netconf"], 5000, noprint)
%%     end,

%%     A = rct_rpc:call(rpc, supervisor, count_children, [sshd_sup], 1000, noprint),
%%     ct:pal("~p",[A]),
%%     B = rct_rpc:call(rpc, ets, all, [], 1000, noprint),
%%     ct:pal("~p",[B]),

%%     BB = length(B),
%%     ct:pal("~p",[BB]),

%%     ok.


add_cmd(Name) ->
    case ct_netconfc:open(Name, [{timeout, 3000}]) of
    	{ok,_} ->
    	    ok;
    	Ret ->
    	    ct:pal("~p, Fail to open NC session: ~p ",[Name, Ret])
    end,

    RbsUnitId = atom_to_list(Name),
    UserLabel = "userLabel_" ++ RbsUnitId,

    case ct_netconfc:edit_config(Name,running,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
					       [{managedElementId,[],["1"]},
						{'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
						 [{'equipmentId',[],["1"]},
						  {'RbsUnit',[{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
						   [{rbsUnitId,[],[RbsUnitId]},
						    {userLabel,[],[UserLabel]} ]}]}]}, 300000) of
	ok ->
	    case ct_netconfc:close_session(Name, 3000) of
	    	ok ->
	    	    ok;
	    	Res ->
	    	    ct:pal("~p, Fail to close session: ~p ",[Name, Res])
	    end;
	    %% ok;
	Res1  ->
	    ct:pal("~p, Fail to add RbsUnit: ~p ",[Name, Res1])
    end.


delete_cmd(Name) ->
    case ct_netconfc:open(Name, [{timeout, 3000}]) of
    	{ok,_} ->
    	    ok;
    	Ret1 ->
    	    ct:pal("~p, Fail to open NC session: ~p ",[Name, Ret1])
    end,

    RbsUnitId1 = atom_to_list(Name),
    UserLabel1 = "userLabel_" ++ RbsUnitId1,

    case ct_netconfc:edit_config(Name,running,{'ManagedElement',
					       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
					       [{managedElementId,[],["1"]},
						{'Equipment',
						 [{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
						 [{'equipmentId',[],["1"]},
						  {'RbsUnit',
						   [{xmlns,"urn:com:ericsson:ecim:rbsunit"},
						    {'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
						    {'nc:operation',"delete"}],
						   [{rbsUnitId,[],[RbsUnitId1]},
						    {userLabel,[],[UserLabel1]}]}]}]}, 300000) of
	ok ->
	    case ct_netconfc:close_session(Name, 3000) of
	    	ok ->
	    	    ok;
	    	Res2 ->
	    	    ct:pal("~p, Fail to close session: ~p ",[Name, Res2])
	    end;
	    %% ok;
	Res3 ->
	    ct:pal("~p, Fail to delete RbsUnit: ~p ",[Name, Res3])
    end.


%%--------------------------------------------------------------------
%% @doc
%% Evaluate memory size from start to end.<br/>
%% @end
%%--------------------------------------------------------------------
evaluate_mem_size(Start_Com_rss, End_Com_rss, Start_Beam_rss, End_Beam_rss) ->

    Com_max_accepted_limit = list_to_integer(Start_Com_rss) + 5000,
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
%% Get CXS label. <br/>
%% Get SW version using COM cli interface. <br/>
%% @end
%%--------------------------------------------------------------------
get_sw_version(Identifier) ->
    ct:pal("### Get SW version",[]),
    rct_cli:connect(Identifier, noprint),
    {ok ,RecievedData} = rct_cli:send(Identifier,"show ManagedElement=1,SystemFunctions=1,SwM=1", noprint),
    %% ct:pal("RecievedData: ~p", [RecievedData]),

    %% Clean upp RecievedData string to a list of strings.
    Var = string:tokens(RecievedData, "=\r\n "),
    %% drop data until you reach "SwVersionMain", the wanted CXS label is the second element.
    [_, CXS | _ ] = lists:dropwhile(fun(X) ->
					    X =/= "SwVersionMain"
				    end, Var),
    ct:pal("CXS: ~p", [CXS]),
    rct_cli:disconnect(Identifier),

    list_to_atom(CXS).

%%--------------------------------------------------------------------
%% @doc
%% get_used_mem_size. <br/>
%% @end
%%--------------------------------------------------------------------
get_used_mem_size(WantedSizes, BeamPid) ->
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
				     snmpd ->
					 Snmpd_Rss = rct_rpc:call(rpc, os, cmd, ["ps -eo fname,rss | egrep '(snmpd)'"], 10000, noprint),
					 ["snmpd", Snmpd_rss] = string:tokens(Snmpd_Rss," \n"),
					 Snmpd_rss
				 end
			 end, WantedSizes),
    %% ct:pal("MemSizes: ~p ",[MemSizes]),
    MemSizes.

get_used_mem_size(WantedSizes, BeamPid, SnmpdPid) ->
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
				     snmpd ->
					 Snmpd_Rss_Data = rct_rpc:call(rpc, os, cmd, ["ps -o fname,rss -p "++SnmpdPid], 10000, noprint),
					 ["COMMAND","RSS","snmpd", Snmpd_rss] = string:tokens(Snmpd_Rss_Data," \n"),
					 Snmpd_rss
				 end
			 end, WantedSizes),
    %% ct:pal("MemSizes: ~p ",[MemSizes]),
    MemSizes.

get_used_mem_size_xxl(WantedSizes, BeamPid, TestNodeBeamPid) ->
    MemSizes = lists:map(fun(X) ->
				 case X of
				     "tot_bc" ->
					 Free = rct_rpc:call(rpc, os, cmd, ["free"], 10000, noprint),
					 {match,[Tot_bc]} =
					     re:run(Free,"buffers/cache: *([0-9]+).*",[{capture,[1],list}]),
					 Tot_bc;
				     "beam" ->
					 Beam_Rss_Data =
					     rct_rpc:call(rpc, os, cmd, ["ps -o fname,rss -p "++BeamPid], 10000, noprint),
					 ["COMMAND","RSS","beam.smp", Beam_rss] =
					     string:tokens(Beam_Rss_Data," \n"),
					 Beam_rss;
				     "test_beam" ->
					 Test_Beam_Rss_Data = rct_rpc:call(rpc, os, cmd, ["ps -o fname,rss -p "++TestNodeBeamPid], 10000, noprint),
					 ["COMMAND","RSS","beam.smp", TestBeam_rss] =
					     string:tokens(Test_Beam_Rss_Data," \n"),
					 TestBeam_rss;
				     ProcName ->
					 Rss_Data =
					     rct_rpc:call(rpc, os, cmd, ["ps -eo fname,rss | egrep "++ProcName], 10000, noprint),
					 case ProcName of
					     "test_oi" ->
						 ["test_oi.", ProcName_rss] = string:tokens(Rss_Data," \n"),
						 ProcName_rss;
					     _ ->
						 [ProcName, ProcName_rss] = string:tokens(Rss_Data," \n"),
						 ProcName_rss
					 end
				 end
			 end, WantedSizes),
    %% ct:pal("MemSizes: ~p ",[MemSizes]),
    MemSizes.

get_pid_mem_usage(PidList, ProcName)->
        MemSizes = lists:map(fun(Pid) ->
				     Rss_Data = rct_rpc:call(rpc, os, cmd, ["ps -o fname,rss -p "++Pid], 10000, noprint),
				     ["COMMAND","RSS", ProcName, MemSize_rss] =
					 string:tokens(Rss_Data," \n"),
				     MemSize_rss
			 end, PidList),
    %% ct:pal("MemSizes: ~p ",[MemSizes]),
    MemSizes.

%%--------------------------------------------------------------------
%% @doc
%% Due to it exist to beam.smp on our node then we need to check which is the correct pid. <br/>
%% get_correct_beam_pid. <br/>
%% @end
%%--------------------------------------------------------------------
get_correct_beam_pid() ->
    %% CorrectBeam = rct_rpc:call(rpc, os, cmd, ["pgrep -fl beam | grep -v testnode "], 5000, noprint),
    %% [BeamPid | _T] = string:tokens(CorrectBeam," \n"),
    BeamPid = rct_rpc:call(rpc, os, getpid, [], 10000, noprint),
    BeamPid.


get_testnode_beam_pid() ->
    TestNodeBeam = rct_rpc:call(rpc, os, cmd, ["pgrep -fl beam | grep testnode "], 10000, noprint),
    [TestNodeBeamPid | _T] = string:tokens(TestNodeBeam," \n"),
    TestNodeBeamPid.


get_pid(ProcName) ->
    ProcPid = rct_rpc:call(rpc, os, cmd, ["pgrep "++ProcName], 10000, noprint),
    Pid = string:tokens(ProcPid, "\n "),
    Pid.

%%--------------------------------------------------------------------
%% @doc
%% Check that all rbsUnits is created. <br/>
%% @end
%%--------------------------------------------------------------------
check_all_rbs_units_is_added(NC_SessionNameList, CLI_SessionNameList) ->
    TotLength = length(NC_SessionNameList)+length(CLI_SessionNameList),
    case length(rct_rpc:call(rpc, ets, tab2list, [rbsUnit], 10000, noprint)) of
	TotLength ->
	    ok;
	_ ->
	    ct:fail("rbsUnits is missing")
    end,
    ok.

%%--------------------------------------------------------------------
% NetConf struff
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% Netconf open sessions. <br/>
%% @end
%%--------------------------------------------------------------------
nc_open_sessions(NC_SessionNameList) ->
        lists:foreach(fun(Name) ->
			  %% ct:pal("### NC Open session Name: ~p", [Name]),
			  {ok,_} = ct_netconfc:open(Name, [{timeout, 30000}])
		  end,
		  NC_SessionNameList).

%%--------------------------------------------------------------------
%% @doc
%% Netconf add rbsUnits. <br/>
%% @end
%%--------------------------------------------------------------------
nc_add_rbsUnits(NC_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  ok = nc_add_rbsunit(Name),
			  %% ct:pal("### Close session: ~p", [Name]),
			  ok = ct_netconfc:close_session(Name, 30000),
			  %% ct:pal("### Open session: ~p", [Name]),
			  {ok,_} = ct_netconfc:open(Name,[{timeout, 30000}])
		  end,
		  NC_SessionNameList).

%%--------------------------------------------------------------------
%% @doc
%% Test netconf create by creating a rbsUnit in FEQM.<br/>
%% @end
%%--------------------------------------------------------------------
nc_add_rbsunit(NC_hookName) ->
    RbsUnitId = atom_to_list(NC_hookName),
    UserLabel = "userLabel_" ++ RbsUnitId,
    %% ct:pal("### Create RBS Unit id: ~p, userlabel: ~p", [RbsUnitId, UserLabel]),

    ok = ct_netconfc:edit_config(NC_hookName,running,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
						      [{managedElementId,[],["1"]},
						       {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
							[{'equipmentId',[],["1"]},
							 {'RbsUnit',[{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
							  [{rbsUnitId,[],[RbsUnitId]},
							   {userLabel,[],[UserLabel]} ]}]}]}, 30000),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Netconf, check rbsUnits exist. <br/>
%% @end
%%--------------------------------------------------------------------
nc_check_rbs_units_is_added(NC_SessionNameList) ->
    lists:foreach(fun(NC_hookName) ->
			  %% ct:pal("Check rbs unit created, id: ~p.",[NC_hookName]),

			  RbsUnitId = atom_to_list(NC_hookName),
			  UserLabel = "userLabel_" ++ RbsUnitId,

			  {ok,[{'ManagedElement',
			  	[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			  	[{managedElementId,[],["1"]},
			  	 {'Equipment',
			  	  [{xmlns,
			  	    "urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
			  	  [{equipmentId,[],["1"]},
			  	   {'RbsUnit',
			  	    [{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
			  	    [{rbsUnitId,[],[RbsUnitId]},
			  	     {userLabel,[],[UserLabel]},
			  	     {hwInstallStatus,[], [_]},
			  	     {administrativeState,[], [_]},
			  	     {operationalState,[], [_]},
			  	     {availState,[], [_]},
			  	     {detectedProductData, _, _},
			  	     {operationalLed, _, _},
			  	     {faultLed, _, _},
			  	     {statusLed, _, _},
			  	     {maintenanceLed, _, _}]
			  	   }]}]}]} =
			      ct_netconfc:get(NC_hookName, {'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
							    [{managedElementId,[],["1"]},
							     {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
							      [{'equipmentId',[],["1"]},
							       {'RbsUnit',  [{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
								[{rbsUnitId,[],[RbsUnitId]}]}
							      ]}]}, 30000)
		  end,
		  NC_SessionNameList),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Netconf delete rbsUnits. <br/>
%% @end
%%--------------------------------------------------------------------
nc_delete_rbsUnits(NC_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  ok = nc_delete_rbsunit(Name),
			  %% ct:pal("### Close session: ~p", [Name]),
			  ok = ct_netconfc:close_session(Name, 30000),
			  %% ct:pal("### Open session: ~p", [Name]),
			  {ok,_} = ct_netconfc:open(Name,[{timeout, 30000}])
		  end,
		  NC_SessionNameList).

%%--------------------------------------------------------------------
%% @doc
%% Test netconf delete by deleteing a rbsUnit in FEQM.<br/><br/>
%% @end
%%--------------------------------------------------------------------
nc_delete_rbsunit(NC_hookName) ->
    RbsUnitId =atom_to_list(NC_hookName),
    UserLabel = "userLabel_" ++ RbsUnitId,
    %% ct:pal("### Delete RBS Unit id: ~p, userlabel: ~p", [RbsUnitId, UserLabel]),

    ok = ct_netconfc:edit_config(NC_hookName,running,{'ManagedElement',
						      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
						      [{managedElementId,[],["1"]},
						       {'Equipment',
							[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
							[{'equipmentId',[],["1"]},
							 {'RbsUnit',
							  [{xmlns,"urn:com:ericsson:ecim:rbsunit"},
							   {'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
							   {'nc:operation',"delete"}],
							  [{rbsUnitId,[],[RbsUnitId]},
							   {userLabel,[],[UserLabel]}]}]}]}, 30000),
    ok.


%% nc_delete_rbsunits(NC_hookName, NC_SessionNameList) ->
%%     lists:foreach(fun(SesionName) ->
%% 			  RbsUnitId =atom_to_list(SesionName),
%% 			  UserLabel = "userLabel_" ++ RbsUnitId,
%% 			  %% ct:pal("### Delete RBS Unit id: ~p, userlabel: ~p", [RbsUnitId, UserLabel]),
%% 			  ct_netconfc:edit_config(NC_hookName,running,{'ManagedElement',
%% 								       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
%% 								       [{managedElementId,[],["1"]},
%% 									{'Equipment',
%% 									 [{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
%% 									      [{'equipmentId',[],["1"]},
%% 									       {'RbsUnit',
%% 										[{xmlns,"urn:com:ericsson:ecim:rbsunit"},
%% 										 {'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
%% 										 {'nc:operation',"delete"}],
%% 										[{rbsUnitId,[],[RbsUnitId]},
%% 										 {userLabel,[],[UserLabel]}]}]}]})
%% 		  end,
%% 		  NC_SessionNameList).


nc_delete_rbsunits_no_check(NC_hookName, NC_SessionNameList) ->
    lists:foreach(fun(SesionName) ->
			  RbsUnitId =atom_to_list(SesionName),
			  UserLabel = "userLabel_" ++ RbsUnitId,
			  ct_netconfc:open(NC_hookName,[]),
			  %% ct:pal("### Delete RBS Unit id: ~p, userlabel: ~p", [RbsUnitId, UserLabel]),
			  ct_netconfc:edit_config(NC_hookName,running,{'ManagedElement',
								       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
								       [{managedElementId,[],["1"]},
									{'Equipment',
									 [{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
									      [{'equipmentId',[],["1"]},
									       {'RbsUnit',
										[{xmlns,"urn:com:ericsson:ecim:rbsunit"},
										 {'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
										 {'nc:operation',"delete"}],
										[{rbsUnitId,[],[RbsUnitId]},
										 {userLabel,[],[UserLabel]}]}]}]}),
			  ct_netconfc:close_session(NC_hookName)
		  end,
		  NC_SessionNameList).

%%--------------------------------------------------------------------
%% @doc
%% Netconf check rbs units does not exist. <br/>
%% @end
%%--------------------------------------------------------------------
nc_check_rbs_units_is_deleted(NC_SessionNameList) ->
    lists:foreach(fun(NC_hookName) ->
			  %% ct:pal("Check rbs unit id: ~p, is deleted.",[NC_hookName]),
			  RbsUnitId = atom_to_list(NC_hookName),

			  {error, [_,
			  	   _,
			  	   _,
			  	   {'error-message', _,  [Res]}
			  	  ]} =
			      ct_netconfc:get(NC_hookName,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
							   [{managedElementId,[],["1"]},
							    {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
							     [{'equipmentId',[],["1"]},
							      {'RbsUnit',  [{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
							       [{rbsUnitId,[],[RbsUnitId]}]}
							     ]}]}, 30000),

			  case re:run(Res, "MO: ManagedElement=1,Equipment=1,RbsUnit=" ++ RbsUnitId ++ " is not available [(]no instance[)].") of
			      {match, _} ->
			  	  %% ct:pal("RbsUnitId: ~p is deleted." ,[RbsUnitId]);
				  ok;
			      nomatch ->
			  	  ct:fail("rbs units still exist")
			  end
    		  end,
    		  NC_SessionNameList),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Netconf close sessions. <br/>
%% @end
%%--------------------------------------------------------------------
nc_close_sessions(NC_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  %% ct:pal("### CloseName: ~p", [Name]),
			  ok = ct_netconfc:close_session(Name, 30000)
		  end,
		  NC_SessionNameList).

%%--------------------------------------------------------------------
%% CLI struff
%%--------------------------------------------------------------------
%% @doc
%% cli create connection. <br/>
%% PrintOpt  print | noprint   Used when wanted printouts in testlog. <br/>
%% @end
%%--------------------------------------------------------------------
cli_create_connection(CLI_SessionNameList) ->
    cli_create_connection(CLI_SessionNameList, noprint).

cli_create_connection(CLI_SessionNameList, PrintOpt) ->
    lists:foreach(fun(Name) ->
			  %% ct:pal("### CLI Connect Name: ~p", [Name]),
			  ok = rct_cli:connect(Name, PrintOpt)
		  end,
		  CLI_SessionNameList).

%%--------------------------------------------------------------------
%% @doc
%% cli add rbsUnits. <br/>
%% PrintOpt  print | noprint   Used when wanted printouts in testlog. <br/>
%% @end
%%--------------------------------------------------------------------
cli_add_rbsUnits(CLI_SessionNameList, PrintOpt) ->
    lists:foreach(fun(Name) ->
			  RbsUnitId = atom_to_list(Name),
			  UserLabel = "userLabel_" ++ RbsUnitId,
			  %% ct:pal("### Add RBS units: ~p", [Name]),
			  rct_cli:send(Name, "configure", "\\(config\\)>", PrintOpt),
			  rct_cli:send(Name, "ManagedElement=1,Equipment=1,RbsUnit="++RbsUnitId, "\\(config-RbsUnit="++RbsUnitId++"\\)>", PrintOpt),
			  %% ct:pal("### Add userlabel: ~p", [Name]),
			  rct_cli:send(Name, "userLabel="++UserLabel, PrintOpt),
			  rct_cli:send(Name, "commit", "\\(config-RbsUnit="++RbsUnitId++"\\)>", PrintOpt),
			  {ok ,RecievedData} = rct_cli:send(Name, "show", PrintOpt),

			  case re:run(RecievedData, UserLabel) of
			      {match, _} ->
				  case re:run(RecievedData, "maintenanceLed") of %% just a check to see that commit was done.
				      {match, _} ->
					  ok;
				      nomatch ->
					  ct:fail("Commit was not done, maintenanceLed field did not exist!")
				  end;
			      nomatch ->
				  ct:fail("User label does not exist")
			  end,
			  rct_cli:send(Name, "top", ">", PrintOpt)
		  end,
		  CLI_SessionNameList).


%%--------------------------------------------------------------------
%% @doc
%% cli delete rbsUnits. <br/>
%% PrintOpt  print | noprint   Used when wanted printouts in testlog. <br/>
%% @end
%%--------------------------------------------------------------------
cli_delete_rbsUnits(CLI_SessionNameList, PrintOpt) ->
    lists:foreach(fun(Name) ->
			  RbsUnitId = atom_to_list(Name),
			  %% ct:pal("### Delete RBS units: ~p", [Name]),
			  rct_cli:send(Name, "configure", "\\(config\\)>", PrintOpt),
			  rct_cli:send(Name, "ManagedElement=1,Equipment=1", "\\(config-Equipment=1\\)>", PrintOpt),
			  rct_cli:send(Name, "no RbsUnit="++RbsUnitId, "\\(config-Equipment=1\\)>", PrintOpt),
			  rct_cli:send(Name, "commit", "\\(config-Equipment=1\\)>", PrintOpt),
			  rct_cli:send(Name, "top", ">", PrintOpt)
		  end,
		  CLI_SessionNameList).

cli_delete_rbsUnits(SessionName, CLI_SessionNameList, PrintOpt) ->
    lists:foreach(fun(Name) ->
			  RbsUnitId = atom_to_list(Name),
			  rct_cli:send(SessionName, "configure", PrintOpt),
			  rct_cli:send(SessionName, "ManagedElement=1,Equipment=1", PrintOpt),
			  rct_cli:send(SessionName, "no RbsUnit="++RbsUnitId, PrintOpt),
			  rct_cli:send(SessionName, "commit", PrintOpt),
			  rct_cli:send(SessionName, "top", PrintOpt)
		  end,
		  CLI_SessionNameList).

%%--------------------------------------------------------------------
%% @doc
%% cli check rbsUnits does not exist. <br/>
%% PrintOpt  print | noprint   Used when wanted printouts in testlog. <br/>
%% @end
%%--------------------------------------------------------------------
cli_check_rbsUnits_deleted(CLI_SessionNameList, PrintOpt) ->
    lists:foreach(fun(Name) ->
			  RbsUnitId = atom_to_list(Name),
			  %% ct:pal("### Check RBS unit: ~p deleted", [Name]),
			  rct_cli:send(Name, "configure", "\\(config\\)>", PrintOpt),
			  rct_cli:send(Name, "ManagedElement=1,Equipment=1", "\\(config-Equipment=1\\)>", PrintOpt),
			  {ok ,RecievedData} = rct_cli:send(Name, "show", PrintOpt),
			  case re:run(RecievedData, "RbsUnit="++RbsUnitId) of
			      nomatch -> ok;
			      {match, _} ->
				  ct:fail("RbsUnit was not deleted. ",[])
			  end,
			  rct_cli:send(Name, "commit", PrintOpt),
			  rct_cli:send(Name, "top", ">", PrintOpt)
		  end,
		  CLI_SessionNameList).

%%--------------------------------------------------------------------
%% @doc
%% cli remove connection. <br/>
%% PrintOpt  print | noprint   Used when wanted printouts in testlog. <br/>
%% @end
%%--------------------------------------------------------------------
cli_remove_connection(CLI_SessionNameList, PrintOpt) ->
    lists:foreach(fun(Name) ->
			  %% ct:pal("### CloseName: ~p", [Name]),
			  ok = rct_cli:disconnect(Name, PrintOpt)
		  end,
		  CLI_SessionNameList).

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
cli_create_conf_snmp() ->
    [NAME|_] = ?CLI_SessionNameList,
    PrintOpt = noprint,
    [{_,DU}] = ct:get_config(test_nodes),
    [{ssh, IP},_ , _, _] = ct:get_config({DU, ssh_lmt_ipv4}),
    {ok, _} = rct_cli:send(NAME, "configure", "\\(config\\)>", PrintOpt),
    {ok, _} = rct_cli:send(NAME, "ManagedElement=1,SystemFunctions=1,SysM=1,Snmp=1", PrintOpt),
    {ok, _} = rct_cli:send(NAME, "administrativeState=UNLOCKED", PrintOpt),

    {ok, _} = rct_cli:send(NAME, "agentAddress", PrintOpt),
    {ok, _} = rct_cli:send(NAME, "host="++IP, PrintOpt),
    {ok, PortConf} =  rct_rpc:call(rpc, sysEnv, get_port_conf, [], 10000, PrintOpt),
    SnmpAgentPort = proplists:get_value(snmpAgent, PortConf),
    {ok, _} = rct_cli:send(NAME, "port=6"++integer_to_list(SnmpAgentPort), PrintOpt),
    {ok, _} = rct_cli:send(NAME, "commit", PrintOpt),
    {ok, _} = rct_cli:send(NAME, "top", PrintOpt),

    lists:foreach(fun(Name) ->
			  SnmpTargetV2C = atom_to_list(Name),
			  {ok, HostName} =inet:gethostname(),
			  {ok, Host_Ip} = inet:getaddr(HostName, inet),
			  HostIp = ip_to_str(Host_Ip),

			  %% {ok, A} = rct_rpc:call(rpc, sysEnv, get_port_conf, [], 10000, noprint),
			  %% {snmpAgent, Port} = lists:keyfind(snmpAgent, 1, A), %% gör om, samma port som trap skickas till!
			  Port=27162,
			  %% ct:pal("### Conf SNMP: ~p", [Name]),
			  {ok, _} = rct_cli:send(Name, "configure", "\\(config\\)>", PrintOpt),
			  {ok, _} = rct_cli:send(Name, "ManagedElement=1,SystemFunctions=1,SysM=1,Snmp=1", PrintOpt),
			  {ok, _} = rct_cli:send(Name, "administrativeState=UNLOCKED", PrintOpt),
			  %% ct:pal("### Add SnmpTargetV2C: ~p", [SnmpTargetV2C]),
			  {ok, _} = rct_cli:send(Name, "SnmpTargetV2C="++SnmpTargetV2C, PrintOpt),
			  {ok, _} = rct_cli:send(Name, "address="++HostIp, PrintOpt),
			  {ok, _} = rct_cli:send(Name, "community=\"public\"", PrintOpt),
			  {ok, _} = rct_cli:send(Name, "port="++integer_to_list(Port), PrintOpt),

			  {ok, _} = rct_cli:send(Name, "commit", PrintOpt),
			  {ok, _} = rct_cli:send(Name, "top", PrintOpt)

		  end,
		  lists:sublist(?CLI_SessionNameList, 5)),
    ok.


ip_to_str({A,B,C,D}) ->
    integer_to_list(A)++"."++
    integer_to_list(B)++"."++
    integer_to_list(C)++"."++
    integer_to_list(D).

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
cli_delete_conf_snmp() ->
    PrintOpt = noprint,

    lists:foreach(fun(Name) ->
			  SnmpTargetV2C = atom_to_list(Name),

			  %% ct:pal("### Delete Conf SNMP: ~p", [Name]),
			  {ok, _} = rct_cli:send(Name, "configure", "\\(config\\)>", PrintOpt),
			  {ok, _} = rct_cli:send(Name, "ManagedElement=1,SystemFunctions=1,SysM=1,Snmp=1", PrintOpt),
			  {ok, _} = rct_cli:send(Name, "no SnmpTargetV2C="++SnmpTargetV2C, PrintOpt),
			  {ok, _} = rct_cli:send(Name, "commit", PrintOpt),
			  {ok, _} = rct_cli:send(Name, "top", PrintOpt)

		  end,
		  lists:sublist(?CLI_SessionNameList,5)),

    [NAME|_] = ?CLI_SessionNameList,
    {ok, _} = rct_cli:send(NAME, "configure", "\\(config\\)>", PrintOpt),
    {ok, _} = rct_cli:send(NAME, "ManagedElement=1,SystemFunctions=1,SysM=1,Snmp=1", PrintOpt),
    {ok, _} = rct_cli:send(NAME, "administrativeState=LOCKED", PrintOpt),

    {ok, _} = rct_cli:send(NAME, "no agentAddress", PrintOpt),
    {ok, _} = rct_cli:send(NAME, "commit", PrintOpt),
    {ok, _} = rct_cli:send(NAME, "top", PrintOpt),

    ok.


cli_delete_conf_snmp(SessionName, CLI_SessionNameList, PrintOpt) ->
    lists:foreach(fun(Name) ->
			  SnmpTargetV2C = atom_to_list(Name),
			  rct_cli:send(SessionName, "configure", "\\(config\\)>", PrintOpt),
			  rct_cli:send(SessionName, "ManagedElement=1,SystemFunctions=1,SysM=1,Snmp=1", PrintOpt),
			  rct_cli:send(SessionName, "no SnmpTargetV2C="++SnmpTargetV2C, PrintOpt),
			  rct_cli:send(SessionName, "commit", PrintOpt),
			  rct_cli:send(SessionName, "top", PrintOpt)
		  end,
		  CLI_SessionNameList),

    rct_cli:send(SessionName, "configure", "\\(config\\)>", PrintOpt),
    rct_cli:send(SessionName, "ManagedElement=1,SystemFunctions=1,SysM=1,Snmp=1", PrintOpt),
    rct_cli:send(SessionName, "administrativeState=LOCKED", PrintOpt),
    rct_cli:send(SessionName, "no agentAddress", PrintOpt),
    rct_cli:send(SessionName, "commit", PrintOpt),
    rct_cli:send(SessionName, "top", PrintOpt),

    ok.

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
cli_check_conf_snmp_created()->
    lists:foreach(fun(Name) ->
			  PrintOpt = noprint,
			  SnmpTargetV2C = atom_to_list(Name),
			  rct_cli:send(Name, "top", PrintOpt),
			  {ok ,RecievedData} = rct_cli:send(Name, "show ManagedElement=1,SystemFunctions=1,SysM=1,Snmp=1,SnmpTargetV2C="++SnmpTargetV2C, PrintOpt),
			  %% ct:pal("~p", [RecievedData]),
			  case re:run(RecievedData, "SnmpTargetV2C="++SnmpTargetV2C) of
			      {match, _} ->
				  case re:run(RecievedData, "operationalState=ENABLED") of
				      {match, _} ->
					  ok;
				      nomatch ->
					  ct:fail("TC fail due to operationalState not ENABLED !")
				      end;
			      nomatch ->
				  ct:fail("TC fail due to SnmpTarget not found!")
			  end
			  %% SnmpTargetV2C_Data = string:tokens(RecievedData, "\""),
			  %% SnmpTargetV2CData = string:tokens(lists:flatten(SnmpTargetV2C_Data), "\r\n "),
			  %% CompleteSnmpTargetV2CData = lists:delete(">", SnmpTargetV2CData),
			  %% ct:pal("~p", [CompleteSnmpTargetV2CData])
			  %% case CompleteSnmpTargetV2CData of
			  %%     ["SnmpTargetV2C="++SnmpTargetV2C,_,_,"operationalState=ENABLED",_] ->
			  %% 	  ok;
			  %%     ["SnmpTargetV2C="++SnmpTargetV2C,_,_,_] ->
			  %% 	  %% ct:pal("Not Enabled",[]),
			  %% 	  timer:sleep(1000),
			  %% 	  cli_check_conf_snmp_created()
			  %% end

			  %% ["SnmpTargetV2C="++SnmpTargetV2C,_,_,_,_] = CompleteSnmpTargetV2CData
		  end,
		  lists:sublist(?CLI_SessionNameList,5)),

    ok.

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
cli_check_conf_snmp_deleted()->
    lists:foreach(fun(Name) ->
    PrintOpt = noprint,
    SnmpTargetV2C = atom_to_list(Name),
    rct_cli:send(Name, "top", PrintOpt),
    {ok ,RecievedData} = rct_cli:send(Name, "show ManagedElement=1,SystemFunctions=1,SysM=1,Snmp=1,SnmpTargetV2C="++SnmpTargetV2C, PrintOpt),
    %% ct:pal("#~p", [RecievedData]),
    case re:run(RecievedData, "ERROR: Specific element not found") of
	{match, _} ->
	    ok;
	nomatch ->
	    ct:fail("TC fail due to snmp config not deleted!")
    end

    end,
    lists:sublist(?CLI_SessionNameList,5)),

    ok.

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
cli_check_alarm_is_active(NrOfAlarms)->
    cli_check_alarm_is_active(?CLI_SessionNameList, NrOfAlarms, 60000).
cli_check_alarm_is_active([], _NrOfAlarms, _Timeout) ->
    ok;
cli_check_alarm_is_active(CLI_SessionNameList, _NrOfAlarms, Timeout) when Timeout < 500 ->
    lists:foreach(fun(Name)->
			 TotActive = rct_cli:send(Name, "show ManagedElement=1,SystemFunctions=1,Fm=1,totalActive", noprint),
			 ct:pal("~p, TotActiveAlarms: ~p",[Name, TotActive])
		 end, CLI_SessionNameList),
    ct:fail(" Alarm is not active within expected time!.");
cli_check_alarm_is_active([Name|T], NrOfAlarms, Timeout)->
    PrintOpt = noprint,
    case rct_cli:send(Name, "show ManagedElement=1,SystemFunctions=1,Fm=1,totalActive", PrintOpt) of
	{ok ,RecievedData} ->
	    %% ct:pal("#active: ~p, Name: ~p", [RecievedData, Name]),
	    case re:run(RecievedData, "totalActive="++NrOfAlarms) of
		{match, _} ->
		    cli_check_alarm_is_active(T, NrOfAlarms, Timeout);
		nomatch ->
		    timer:sleep(500),
		    cli_check_alarm_is_active([Name]++T, NrOfAlarms, Timeout-500)
	    end
    end.

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
cli_check_alarm_is_inactive()->
    cli_check_alarm_is_inactive(?CLI_SessionNameList, 60000).
cli_check_alarm_is_inactive([], _Timeout) ->
    ok;
cli_check_alarm_is_inactive(CLI_SessionNameList, Timeout) when Timeout < 500 ->
    lists:foreach(fun(Name)->
			 TotActive = rct_cli:send(Name, "show ManagedElement=1,SystemFunctions=1,Fm=1,totalActive", noprint),
			 ct:pal("~p, TotActiveAlarms: ~p",[Name, TotActive])
		 end, CLI_SessionNameList),
    ct:fail(" Alarm is not inactive within expected time!.");
cli_check_alarm_is_inactive([Name|T], Timeout)->
    PrintOpt = noprint,
    case rct_cli:send(Name, "show ManagedElement=1,SystemFunctions=1,Fm=1,totalActive", PrintOpt) of
	{ok ,RecievedData} ->
	    %% ct:pal("#inactive: ~p, Name: ~p", [RecievedData, Name]),
	    %% case re:run(RecievedData, "Attribute 'totalActive' not set") of
	    case re:run(RecievedData, "totalActive=0") of
		{match, _} ->
		    cli_check_alarm_is_inactive(T, Timeout);
		nomatch ->
		    timer:sleep(500),
		    cli_check_alarm_is_inactive([Name]++T, Timeout-500)
	    end
    end.


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
    			  [] = rct_rpc:call(rpc, os, cmd, [?CPULIMIT_TO_PATH++"cpulimit -z -i -l "++LoadProcent++" cat /dev/zero > /dev/null &"], 10000, noprint)
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

    CpuLimitPids = rct_rpc:call(rpc, os, cmd, ["pgrep cpulimit"], 10000, noprint),
    %% Build a List with Pids, remove \n from the received list.
    CpuLimitPidList = string:tokens(CpuLimitPids, "\n"),
    ct:pal("CpuLimitPidList:~p", [CpuLimitPidList]),

    {ok, CpuLoad} = rct_tlib:cpuload(identifier,100),
    lists:foreach(fun({_, Load})->
    			  case Load > 30 of
    			      true ->
    				  ok;
    			      false ->
    				  ct:pal("Fail:~p", [CpuLoad]),
    				  ct:fail(" Background load is lower than expected! ")
    			  end
    		  end, CpuLoad),
    ok.

%%% #0.    BASIC INFORMATION
%% coding: latin-1
%%% ----------------------------------------------------------
%%% %CCaseFile:	meas_mem_usage_alarm_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R5A/2
%%% 
%%% @doc == Measure and check memory size when generate/cease alarms via lih.==
%%% This Test Suite can be used on target and simulated enviroment.<br/>
%%%
%%% 
%%% @end

-module(meas_mem_usage_alarm_SUITE).
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
%%% R2A/1      2013-08-21 etxivri     Created
%%% R2A/3      2013-08-21 etxivri     Added 1 hour.
%%% R2A/3      2013-08-21 etxivri     Updated some edoc.
%%% R2A/5      2013-08-26 etxivri     Added printout of disc usage.
%%% R2A/6      2013-10-01 etxivri     Changed cli command end to top, due to changes in com bd8.
%%% R5A/1      2016-01-21 erarafo     Fixed deprecation warning and copyright
%%% R5A/2      2016-02-12 etxkols     Changed rct_rpc:call timeouts to 10000 millisecs
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
	 meas_mem_usage_alarm_1_hour/1,
	 meas_mem_usage_alarm_10_hour/1,
	 meas_mem_usage_alarm_150_hour/1
	]).

-define(NrOfCliUsers, 1).
%% -define(NrOfCliUsers, 5).

-define(CLI_SessionNameList, [list_to_atom("cli"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfCliUsers)]).

%% -define(ALL_ALARM_MEASURED_FILE, "all_alarm_measured_memory_file.txt").
-define(ALL_ALARM_MEASURED_FILE_1, "all_alarm_measured_memory_file_1.txt").
-define(ALL_ALARM_MEASURED_FILE_2, "all_alarm_measured_memory_file_2.txt").

-define(All_MEASURED_LOGS, "all_meas_mem_usage_alarm_log.txt").

-define(LOG_DIR, "/proj/rcs/measurements/").
%% -define(LOG_DIR, "/home/etxivri/tmp/").

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% runs the rct_netconf hook for each user, to be able to open a netconf session for each user.<br/>
%% @end
%%--------------------------------------------------------------------
suite() -> 
    %% build up a list of NetconfHooks touples with differents Names.
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
		 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}} | CliHooks
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
    	    ct:pal("Testcase failed due to: ~p.  \nClean up!", [Reason]),

	    A = rct_rpc:call(rpc, os, cmd, ["pgrep com"], 10000, noprint),
	    B = rct_rpc:call(rpc, os, cmd, ["pgrep cli"], 10000, noprint),
	    BB = length(string:tokens(B,"\n")),
	    ct:pal("### Com: ~p",[A]),
	    ct:pal("### Cli: ~p",[BB]),	    

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
	    %% rct_rpc:call(rpc, lih_fm, cancel_emergency_unlock_alarm, [], 10000, noprint),
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

cli_connect({error,already_connected}, Name) ->
    rct_cli:disconnect(Name, noprint), % Clean up at our client, if cli process was killed on node, 
    rct_cli:connect(Name, noprint), % Then set up again.
    cli_delete_conf_snmp(Name, ?CLI_SessionNameList, noprint),
    throw({?MODULE, found});
cli_connect(ok, Name) ->
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
%% calls do_meas_mem_alarm_no_check with numbers of Hours that will perform the actual test.
%% @spec meas_mem_usage_alarm_1_hour(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
meas_mem_usage_alarm_1_hour(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_alarm_no_check(1, LogPath).

%%--------------------------------------------------------------------
%% @doc
%% calls do_meas_mem_alarm_no_check with numbers of Hours that will perform the actual test.
%% @spec meas_mem_usage_alarm_10_hour(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
meas_mem_usage_alarm_10_hour(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_alarm_no_check(10, LogPath).

%%--------------------------------------------------------------------
%% @doc
%% calls do_meas_mem_alarm_no_check with numbers of Hours that will perform the actual test.
%% @spec meas_mem_usage_alarm_150_hour(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
meas_mem_usage_alarm_150_hour(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_alarm_no_check(150, LogPath).

%%--------------------------------------------------------------------
%% @doc
%% @spec do_meas_mem_alarm_no_check(Hours, LogPath) -> ok
%% @end
%%--------------------------------------------------------------------
do_meas_mem_alarm_no_check(Hours, LogPath) ->
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

    timer:sleep(10000), 
    TestNodeBeamPid = get_testnode_beam_pid(),
    ct:pal("TestNodeBeamPid: ~p .",[TestNodeBeamPid]),

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
    timer:sleep(5000), %% wait for operationalState=ENABLED !, 
    cli_check_conf_snmp_created(),
    timer:sleep(5000), %% wait!, Pid will be changed at startup!
    SnmpdPidB = string:tokens(rct_rpc:call(rpc, os, cmd, ["pgrep snmpd"], 10000, noprint), "\n "),
    case SnmpdPidA of
	[] ->
	    SnmpdPid = SnmpdPidB;
	_ ->
	    SnmpdPid = lists:delete(lists:flatten(SnmpdPidA), SnmpdPidB)
    end,
    ct:pal("snmpd Pid before config snmp: ~p",[SnmpdPidA]),
    ct:pal("snmpd Pid after config snmp: ~p",[SnmpdPidB]),
    %% ct:pal("C: ~p",[SnmpdPid]),

    ct:pal("#LogPath_1# ~p", [LogPath++?ALL_ALARM_MEASURED_FILE_1]),
    ct:pal("#LogPath_2# ~p", [LogPath++?ALL_ALARM_MEASURED_FILE_2]),

    %%%%
    %%get start memory size
    %%%%
    [Start_Tot_bc,
     Start_Com_rss, 
     Start_Beam_rss, 
     Start_TestBeam_rss,
     Start_Snmpd_rss] = get_used_mem_size_xxl(["tot_bc", 
					       "com", 
					       "beam", 
					       "test_beam",
					       "snmpd"], BeamPid, TestNodeBeamPid, SnmpdPid),

    %%%%
    %% Generate / Cease Alarm.
    %%%%
    StartTime = erlang:timestamp(),

    {End_Tot_bc,
     End_Com_rss, 
     End_Beam_rss, 
     End_TestBeam_rss,
     End_Snmpd_rss, 
     Nr} = loop_gen_cease_alarm_check_mem(StartTime, 
					  Seconds,
					  %% 120, 
					  Start_Tot_bc,
					  Start_Com_rss, 
					  Start_Beam_rss,
					  Start_TestBeam_rss,
					  Start_Snmpd_rss,
					  LogPath,
					  BeamPid,
					  TestNodeBeamPid, 
					  SnmpdPid),

    FileName = "meas_mem_usage_alarm_"++ integer_to_list(Hours) ++"_hour",
    rct_tlib:writeDataToFile(?LOG_DIR, FileName, "~p;~w;~p;~s;~s;~p;~s;~s;~p;~s;~s;~p;~s;~s;~p;~s;~s;~p;~w ~n", 
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
			      Start_TestBeam_rss,
			      End_TestBeam_rss,
			      list_to_integer(End_TestBeam_rss) - list_to_integer(Start_TestBeam_rss),
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
    %% check if new snmpd exist.
    %%%%
    SnmpdPid2 = string:tokens(rct_rpc:call(rpc, os, cmd, ["pgrep snmpd"], 10000, noprint), "\n "),
    ct:pal("Start_SnmpdPid: ~p",[SnmpdPid]),
    ct:pal("End_SnmpdPid: ~p",[SnmpdPid2]),
    %% SnmpdPidB=SnmpdPid2,

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

    %%%%
    %% check cli connection deleted
    %%%%
    ct:pal("Check nc session and cli connections is deleted.",[]),
    [] = rct_rpc:call(rpc, os, cmd, ["pgrep cli"], 10000, noprint),

    ct:pal("All measured memory is loged at:~n"
	   "LogPath: ~p, ~n"
	   "filename 1:  ~p ~n"
	   "filename 2:  ~p ~n",[LogPath, ?ALL_ALARM_MEASURED_FILE_1, ?ALL_ALARM_MEASURED_FILE_2]),

    ok.



%%--------------------------------------------------------------------
%% @doc 
%% Loop alarm operations during a time intervall. <br/>
%% Chek memory size. <br/>
%% @end
%%--------------------------------------------------------------------
loop_gen_cease_alarm_check_mem(StartTime, Duration, Start_Tot_bc, Start_Com_rss, Start_Beam_rss, Start_TestBeam_rss, Start_Snmpd_rss, LogPath, BeamPid, TestNodeBeamPid, Snmpd) ->
    loop_gen_cease_alarm_check_mem(StartTime, Duration, 0, Start_Tot_bc, Start_Com_rss, Start_Beam_rss, Start_TestBeam_rss, Start_Snmpd_rss, LogPath, BeamPid, TestNodeBeamPid, Snmpd).

loop_gen_cease_alarm_check_mem(StartTime, Duration, Nr, Start_Tot_bc, Start_Com_rss, Start_Beam_rss, Start_TestBeam_rss, Start_Snmpd_rss, LogPath, BeamPid, TestNodeBeamPid, Snmpd) ->
    %% ct:pal("¤¤¤ generate / cease alarm, nr: ~p", [Nr]),
    %% test_server:break("break"),

    try

    %%%%
    %% Generate alarm.
    %%%%
    %% ct:pal("Generate alarms.",[]),
    rct_rpc:call(rpc, lih_fm, request_LKF_fault_alarm, [], 10000, noprint),
    rct_rpc:call(rpc, lih_fm, request_LKF_missing_alarm, [], 10000, noprint),
    rct_rpc:call(rpc, lih_fm, request_emergency_unlock_alarm, [], 10000, noprint),
    rct_rpc:call(rpc, lih_fm, request_emergency_unlock_expired_alarm, [], 10000, noprint),
    rct_rpc:call(rpc, lih_fm, request_integration_unlock_alarm, [], 10000, noprint),
    rct_rpc:call(rpc, lih_fm, request_integration_unlock_expired_alarm, [], 10000, noprint),
    rct_rpc:call(rpc, lih_fm, request_production_unlock_alarm, [], 10000, noprint),
    rct_rpc:call(rpc, lih_fm, request_production_unlock_expired_alarm, [], 10000, noprint),
    %% %%%%
    %% %% Check alarm is active.
    %% %%%%
    NeOfAlarms="8",
    cli_check_alarm_is_active(NeOfAlarms), %% Will not fail it is unexpected.

    %%%%
    %% Cease alarm.
    %%%%
    %% ct:pal("Cease alarms.",[]),
    rct_rpc:call(rpc, lih_fm, cancel_LKF_fault_alarm, [], 10000, noprint),
    rct_rpc:call(rpc, lih_fm, cancel_LKF_missing_alarm, [], 10000, noprint),
    rct_rpc:call(rpc, lih_fm, cancel_emergency_unlock_alarm, [], 10000, noprint),
    rct_rpc:call(rpc, lih_fm, cancel_emergency_unlock_expired_alarm, [], 10000, noprint),
    rct_rpc:call(rpc, lih_fm, cancel_integration_unlock_alarm, [], 10000, noprint),
    rct_rpc:call(rpc, lih_fm, cancel_integration_unlock_expired_alarm, [], 10000, noprint),
    rct_rpc:call(rpc, lih_fm, cancel_production_unlock_alarm, [], 10000, noprint),
    rct_rpc:call(rpc, lih_fm, cancel_production_unlock_expired_alarm, [], 10000, noprint),
    %% %%%%
    %% %% Check alarm is not active.
    %% %%%%
    cli_check_alarm_is_inactive()  %% Will not fail it is unexpected.

    catch
	_:Reason ->
	    ct:pal("Catched, Reason: ~p", [Reason])
    end,
	    

    %%%%
    %% Get mem size and evaluate.
    %%%%
    [End_Tot_bc, 
     End_Com_rss, 
     End_Beam_rss,
     End_TestBeam_rss,
     End_Snmpd_rss] = get_used_mem_size_xxl(["tot_bc", 
					     "com", "beam", 
					     "test_beam", 
					     "snmpd"], BeamPid, TestNodeBeamPid, Snmpd),

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


	    ct:log("~p; Nr: ~p; Time: ~p;  NoOfProc: ~p;~n"
	    	   "Start_tot_bc: ~s; End_tot_bc: ~s; diff_tot_bc: ~p ; ~n "
		   "Start_com: ~s; End_com: ~s; diff_com: ~p ; ~n"
		   "Start_beam: ~s; End_beam: ~s; diff_beam: ~p ; ~n"
		   "Start_snmpd: ~s; End_snmpd: ~s; diff_snmpd: ~p ; ~n"
		   "Start_TestBeam: ~s; End_TestBeam: ~s; diff_TestBeam: ~p ~n",
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
		    Start_Snmpd_rss,
		    End_Snmpd_rss,
		    list_to_integer(End_Snmpd_rss) - list_to_integer(Start_Snmpd_rss),
		    Start_TestBeam_rss,
		    End_TestBeam_rss,
		    list_to_integer(End_TestBeam_rss) - list_to_integer(Start_TestBeam_rss)
		   ]),

	    DF = rct_rpc:call(rpc, os, cmd, ["df"], 10000, noprint),
	    Df = string:tokens(DF, " \n"),
	    ct:log("### Df: ~p", [Df]),
	    DiskNr = string:str(Df, ["/disk"]),
	    %% ct:pal("### DiskNr: ~p", [DiskNr]),
	    UsedDisk = lists:nth(DiskNr-1, Df),
	    %% ct:pal("### Used Disk : ~p", [UsedDisk]),
    
	    
	    %% Logs will be stored in test log path.
	    rct_tlib:writeDataToFile(LogPath, ?ALL_ALARM_MEASURED_FILE_1, "~p; Nr: ~p; "
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

	    rct_tlib:writeDataToFile(LogPath, ?ALL_ALARM_MEASURED_FILE_2, "~p; Nr: ~p; "
				     "Start_TestBeam: ~s; End_TestBeam: ~s; diff_TestBeam: ~p; "
				     "UsedDisc: ~p ~n",
	    			     [httpd_util:rfc1123_date(),
	    			      Nr,
				      Start_TestBeam_rss,
				      End_TestBeam_rss,
				      list_to_integer(End_TestBeam_rss) - list_to_integer(Start_TestBeam_rss),
				      UsedDisk
				     ]);

	_ ->
	    ok
    end,

    CheckTime = os:timestamp(),
    TimeDiff = trunc(timer:now_diff(CheckTime, StartTime) / 1000 / 1000),
    %% ct:pal("TimeDiff: ~p", [TimeDiff]),
    case TimeDiff > Duration of
	true ->
	    {End_Tot_bc, 
	     End_Com_rss, 
	     End_Beam_rss, 
	     End_TestBeam_rss,
	     End_Snmpd_rss, Nr};
	false ->
	   loop_gen_cease_alarm_check_mem(StartTime, Duration, Nr+1, Start_Tot_bc, Start_Com_rss, Start_Beam_rss, Start_TestBeam_rss, Start_Snmpd_rss, LogPath, BeamPid, TestNodeBeamPid, Snmpd)
    end.






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
get_used_mem_size_xxl(WantedSizes, BeamPid, TestNodeBeamPid, SnmpdPid) ->
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
				     "snmpd" ->
					 Snmpd_Rss_Data = rct_rpc:call(rpc, os, cmd, ["ps -o fname,rss -p "++SnmpdPid], 10000, noprint),
					 ["COMMAND","RSS","snmpd", Snmpd_rss] = string:tokens(Snmpd_Rss_Data," \n"),
					 Snmpd_rss;
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

%% get_pid_mem_usage(PidList, ProcName)->
%%         MemSizes = lists:map(fun(Pid) ->
%% 				     Rss_Data = rct_rpc:call(rpc, os, cmd, ["ps -o fname,rss -p "++Pid], 10000, noprint),
%% 				     ["COMMAND","RSS", ProcName, MemSize_rss] = 
%% 					 string:tokens(Rss_Data," \n"),
%% 				     MemSize_rss
%% 			 end, PidList),
%%     %% ct:pal("MemSizes: ~p ",[MemSizes]),
%%     MemSizes.

%%--------------------------------------------------------------------
%% @doc 
%% Due to it exist to beam.smp on our node then we need to check which is the correct pid. <br/>
%% get_correct_beam_pid. <br/>
%% @end
%%--------------------------------------------------------------------	    
get_correct_beam_pid() ->
    %% CorrectBeam = rct_rpc:call(rpc, os, cmd, ["pgrep -fl beam | grep -v testnode "], 10000, noprint),
    %% [BeamPid | _T] = string:tokens(CorrectBeam," \n"),
    BeamPid = rct_rpc:call(rpc, os, getpid, [], 10000, noprint),
    BeamPid.


get_testnode_beam_pid() ->
    TestNodeBeam = rct_rpc:call(rpc, os, cmd, ["pgrep -fl beam | grep testnode "], 10000, noprint),
    [TestNodeBeamPid | _T] = string:tokens(TestNodeBeam," \n"),
    TestNodeBeamPid.


%% get_pid(ProcName) ->
%%     ProcPid = rct_rpc:call(rpc, os, cmd, ["pgrep "++ProcName], 10000, noprint),
%%     Pid = string:tokens(ProcPid, "\n "),
%%     Pid.

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
		  %% lists:sublist(?CLI_SessionNameList, 5)),
		  ?CLI_SessionNameList),

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
		  %% lists:sublist(?CLI_SessionNameList,5)),
		  ?CLI_SessionNameList),

    [NAME|_] = ?CLI_SessionNameList,
    {ok, _} = rct_cli:send(NAME, "configure", "\\(config\\)>", PrintOpt),
    {ok, _} = rct_cli:send(NAME, "ManagedElement=1,SystemFunctions=1,SysM=1,Snmp=1", PrintOpt),
    {ok, _} = rct_cli:send(NAME, "administrativeState=LOCKED", PrintOpt),

    {ok, _} = rct_cli:send(NAME, "no agentAddress", PrintOpt), 
    {ok, _} = rct_cli:send(NAME, "commit", PrintOpt), 
    {ok, _} = rct_cli:send(NAME, "top", PrintOpt),

    ok.

%% Used in cleanup!
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
    cli_check_conf_snmp_created(30000).

cli_check_conf_snmp_created(Timeout) when Timeout < 100 ->
    ct:fail("TC fail due to operationalState not ENABLED within expected time!");

cli_check_conf_snmp_created(Timeout) ->
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
				      nomatch ->  %% wait for opstate to be enable
					  timer:sleep(1000),
					  cli_check_conf_snmp_created(Timeout-1000)
				      end;
			      nomatch -> 
				  ct:fail("TC fail due to SnmpTarget not found!")
			  end  
		  end, 
		  %% lists:sublist(?CLI_SessionNameList,5)),
		  ?CLI_SessionNameList),

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
    %% lists:sublist(?CLI_SessionNameList,5)),
		  ?CLI_SessionNameList),
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
			 ct:pal("Unexpected nr of active alarms: ~p, TotActiveAlarms: ~p",[Name, TotActive])
		 end, CLI_SessionNameList);
    %% ct:fail(" Alarm is not active within expected time!.");
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
	    end;
	{error,closed} ->
	    ct:log("cli connection closed, connect it again.", []),
	    rct_cli:connect(Name, noprint),
	    timer:sleep(1000),
	    cli_check_alarm_is_active([Name]++T, NrOfAlarms, Timeout-1000);
	Ret ->
	    ct:pal("Alarm is active, unexpected return value from re: ~p", [Ret]),
	    timer:sleep(500),
	    cli_check_alarm_is_active([Name]++T, NrOfAlarms, Timeout-500)
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
			 ct:pal("Unexpected nr of inactive alarms: ~p, TotActiveAlarms: ~p",[Name, TotActive])
		 end, CLI_SessionNameList);
    %% ct:fail(" Alarm is not inactive within expected time!.");
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
	    end;
	{error,closed} ->
	    ct:log("cli connection closed, connect it again.", []),
	    rct_cli:connect(Name, noprint),
	    timer:sleep(1000),
	    cli_check_alarm_is_inactive([Name]++T, Timeout-1000);
	Ret ->
	    ct:pal("Alarm is inactive, unexpected return value from re: ~p", [Ret]),
	    timer:sleep(500),
	    cli_check_alarm_is_inactive([Name]++T, Timeout-500)
    end.

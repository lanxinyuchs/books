%%% #0.    BASIC INFORMATION
%% coding: latin-1
%%% ----------------------------------------------------------
%%% %CCaseFile:	measure_memory_imm_oi_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R3A/R4A/2
%%% 
%%% @doc == Measure memory usages when netconf users operate on a fragment that belongs to application that is Object Implementor.==
%%% This Test Suite can be used on target enviroment.
%%% Used sw version and measured data is written to a file.
%%% Path: /proj/rcs/measurements/
%%%
%%% <br/><br/>
%%% 
%%% @end

-module(measure_memory_imm_oi_SUITE).
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
%%% R2A/2      2013-05-08 etxivri     Created
%%% R2A/3      2013-05-15 etxivri     updates for xxl test.
%%% R2A/4      2013-05-16 etxivri     Added cli operations. Now you can chose operation type,
%%%                                   Netconf, cli or both.
%%% R2A/5      2013-05-20 etxivri     Added result from top cmd is printed out when tc end.
%%%                                   And some minor updates.
%%% R2A/6      2013-08-13 etxkols     Changed raw to no_prompt_check in ct_telnet:expect
%%% R2A/8      2013-08-27 etxivri     Added discusage printed out in ct log. 
%%% R2A/9      2013-08-30 etxivri     Added timeout value in nc operations.
%%% R2A/10     2013-08-30 etxivri     Updates when both nc and cli is used. And a try to make it more robust.
%%% R2A/11     2013-09-05 etxivri     More updates to make nc sessions more robust. Changed to a better pgrep com.
%%% R2A/12     2013-09-10 etxivri     Updates due to renamed saf-rpc hooks.
%%% R2A/13     2013-09-60 etxivri     Changed some ct:pal to ct:log.
%%% R2A/15     2013-10-01 etxivri     Changed cli command end to top, due to changes in com bd8.
%%% R2A/15     2013-10-01 etxivri     Added TC with nr of repetitions to be executed.
%%% R2A/16     2013-10-25 etxivri     Added new TCs using Nr of repetitions.
%%% R2A/17     2013-10-25 etxivri     Updates in loop printout.
%%% R2A/18     2013-11-07 etxivri     Updates to make cpulimit works on ARM.
%%% R2A/19     2013-11-29 etxivri     Added HW type on File name, 
%%%                                   to separate the measured files.
%%% R2A/20     2014-01-16 etxkols     Support for dus5201. 
%%% R2A/21     2014-03-06 etxivri     Update add branch to filename.
%%%                                   if branch is R2A then old filename is use.
%%% R2A/22     2014-06-04 etxivri     Changed check for "du1 login" prompt to 
%%%                                   "login:"
%%% R2A/24     2014-07-08 etxivri     Update due to limit of nr off ssh session
%%%                                    is set to 5 at same time.
%%% R2A/25     2014-08-22 etxivri     Update to get disc use on ARM
%%% R2A/26     2014-09-12 etxivri     Update to get disc use. 
%%%                                   And mowed initi oi to init and end pert tc
%%% R2A/28     2014-09-18 etxivri     Update due to limit of nr of ssh chanels at same time.
%%% R3A/1      2014-12-04 etxivri     Update grt sw version.
%%% R3A/2      2014-12-04 etxivri     Remove unnecessary reboot.
%%% R3A/4      2015-03-17 etxivri     Make open cli connection more robust.
%%% R4A/1      2015-10-06 etxmlar     Allow 2 CPU:s to have lower background load
%%%                                   than expected.
%%% R4A/2      2016-02-12 etxkols     Changed rct_rpc:call timeouts to 10000 millisecs
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
	 %% Netconf
	 meas_mem_nc_operation_imm_oi_1_hour/1,
	 meas_mem_nc_operation_imm_oi_1_hour_backgroundload/1,
	 meas_mem_nc_operation_imm_oi_10_hour/1,
	 meas_mem_nc_operation_imm_oi_50_hour/1,
	 meas_mem_nc_operation_imm_oi_150_hour/1,
	 meas_mem_nc_operation_imm_oi_150_hour_backgroundload/1,
	 %% Cli
	 meas_mem_cli_operation_imm_oi_1_hour/1,
	 meas_mem_cli_operation_imm_oi_10_hour/1,
	 meas_mem_cli_operation_imm_oi_50_hour/1,
	 meas_mem_cli_operation_imm_oi_150_hour_backgroundload/1,
	 %% Netconf and Cli
	 meas_mem_nc_cli_operation_imm_oi_1_hour/1,
	 meas_mem_nc_cli_operation_imm_oi_10_hour/1,
	 meas_mem_nc_cli_operation_imm_oi_50_hour/1,
	 meas_mem_nc_cli_operation_imm_oi_150_hour_backgroundload/1,
	 %% Netconf and Cli Nr of repetitions, 50 rep is about 1 hour
	 meas_mem_50_nc_operation_imm_oi/1,
	 meas_mem_50_cli_operation_imm_oi/1,
	 meas_mem_50_nc_cli_operation_imm_oi/1
	]).


%% -define(CLI_Name, cli1). %% CLI hook name

-define(NrOfNetConfUsers, 5).
%% -define(NrOfNetConfUsers, 20).
-define(NC_SessionNameList, [list_to_atom("nc"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfNetConfUsers)]).

-define(NrOfCliUsers, 5).
%% -define(NrOfCliUsers, 20).

-define(CLI_SessionNameList, [list_to_atom("cli"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfCliUsers)]).

-define(All_OPERATION_IMM_OI_MEASURED_FILE_1, "all_operation_imm_oi_measured_memory_file_1.txt").
-define(All_OPERATION_IMM_OI_MEASURED_FILE_2, "all_operation_imm_oi_measured_memory_file_2.txt").

-define(LOG_DIR, "/proj/rcs/measurements/meas_mem_nc_cli_operations/").
%% -define(LOG_DIR, "/home/etxivri/tmp/").

-define(CPULIMIT_FROM_PATH, "/proj/rcs/misc/cpulimit").
-define(CPULIMIT_FROM_PATH_ARM, " /proj/rcs/misc/cpulimit_arm/cpulimit").
-define(CPULIMIT_TO_PATH, "/home/sirpa/dev_patches/").

-define(IMPL_NAME_1, "ImplementerOne").
-define(IMM_TEST_CLASS1, "TESTMOMTestClass1").
-define(INSTANCE_NAME, instance_1).


%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% @end
%%--------------------------------------------------------------------
suite() -> 
    %% build up a list of NetconfHooks touples with differents Names.
    NetconfHooks = [{rct_netconf, list_to_atom("nc"++integer_to_list(N))} || N <- lists:seq(1,?NrOfNetConfUsers)],
    CliHooks = [{rct_cli, {list_to_atom("cli"++integer_to_list(N)), [manual_connect]}} || N <- lists:seq(1,?NrOfCliUsers)],

    [{timetrap, {hours, 200}}, % 10 hours
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 %% {cth_conn_log, []},
		 {rct_consserv,cs1},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT",
		 				"CRASH REPORT"],[]}}]}},
		 %% {rct_logging, 
		 %%  {oi_testapp, [{erlang,{["ERROR REPORT",
		 %% 			  "CRASH REPORT"],[]}}]}},
                 {rct_rs232,console},
		 {rct_cli, {cli1, [manual_connect]}},
		 {rct_safe_rpc,[{safe_services, 
				 [{imm, 0}]}, %% debuglevel 0
				{instance_name, rct_safe_imm_oi_rpc}, 
				{finalize_on_fail, true}]},
		 {rct_tlib,{identifier,[]}},
		 {rct_scp, [{1, node1}]},
                 %% {rct_core,[]} | NetconfHooks
		 {rct_core,[]} | lists:append(NetconfHooks, CliHooks)
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
    %% Connect a OI for the testfragment
    %%%%
    ct:pal("Initialize OI Handle", []),
    {ok, Handle, Vsn} = rct_safe_imm_oi_rpc:initialize_2( ok),
    ct:pal("Initialize ok: ~p ~p",[Handle, Vsn]),
    ct:pal("Set Implementer name: ~p", [?IMPL_NAME_1]),
    ok = rct_safe_imm_oi_rpc:implementer_set(Handle, ?IMPL_NAME_1),
    ct:pal("Set Class Implementer for: ~p", [?IMM_TEST_CLASS1]),
    ok = rct_safe_imm_oi_rpc:class_implementer_set(Handle, ?IMM_TEST_CLASS1),
    [{handle, Handle}| Config].
    %% Config.


%% @hidden
end_per_testcase(_TestCase, Config) ->
    ct:pal("End per TC.", []),

    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p.  \nClean up.", [Reason]),

	    A = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 10000, noprint),
	    B = rct_rpc:call(rpc, os, cmd, ["pgrep cli"], 10000, noprint),
	    C = rct_rpc:call(rpc, os, cmd, ["pgrep netconf"], 10000, noprint),
	    D = rct_rpc:call(rpc, os, getpid, [], 10000),

	    ct:pal("### Com: ~p",[A]),
	    ct:pal("### Cli: ~p",[B]),
	    ct:pal("### Netconf: ~p",[C]),
	    ct:pal("### BeamPid: ~p .",[D]),


	    OI = rct_rpc:call(rpc, safs_imm_db, ci_get, ['TESTMOMTestClass1'], 10000, noprint),
	    ct:pal("~p.", [OI]),

	    %%%%%%%%%%%%%%
	    %% Clean up Netconf configurations!
	    %%%%%%%%%%%%%%
	    %%%%
	    %% Open a netconf session and clean up rbsUnits.
            %%%%
	    try
	    	lists:foreach(fun(Name) -> nc_open(ct_netconfc:open(Name, [{timeout, 60000}]), Name) end, ?NC_SessionNameList)
	    catch throw:{?MODULE, found} ->
	    	    ok
	    end,

            %%%%
	    %% Close nc sessions
            %%%%
	    lists:foreach(fun(Name1) ->
	    			  %% ct:pal("### Cleanup after fail TC, Close session: ~p", [Name1]),
	    			  ct_netconfc:close_session(Name1, 30000)
	    		  end, 
	    		  ?NC_SessionNameList),
	    
	    %%%%%%%%%%%%%%
	    %% Clean up cli configurations!
	    %%%%%%%%%%%%%%
	    %%%%
	    %% Open a cli connection and clean up.
            %%%%
	    try
	    	lists:foreach(fun(Name2) -> cli_connect(rct_cli:connect(Name2, noprint), Name2) end, ?CLI_SessionNameList)
	    catch throw:{?MODULE, found} ->
	    	    ok
	    end,
	    
	    %%%%
	    %% Close cli connection
	    %%%%
	    lists:foreach(fun(Name3) ->
	    			  %% ct:pal("### Cleanup after fail TC, CloseName: ~p", [Name3]),
	    			  rct_cli:disconnect(Name3, noprint)
	    		  end, 
	    		  ?CLI_SessionNameList),
	    
	    %%%%
	    %% Delete OI for the testfragment
            %%%%
	    Handle = proplists:get_value(handle, Config),
	    ct:pal("Release Class Implementer for: ~p", [?IMM_TEST_CLASS1]),
	    %% ok = rct_safe_imm_oi_rpc:class_implementer_release(Handle, ?IMM_TEST_CLASS1),
	    rct_safe_imm_oi_rpc:class_implementer_release(Handle, ?IMM_TEST_CLASS1),
	    ct:pal("~p",[A]),
	    ct:pal("Clear Implementer ~p", [?IMPL_NAME_1]),
	    rct_safe_imm_oi_rpc:implementer_clear(Handle),
	    ct:pal("Finalize OI Handle ~p", [Handle]),
	    rct_safe_imm_oi_rpc:finalize(Handle)
		
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


nc_open({error,_}, Name) ->
    ct_netconfc:close_session(Name, 30000), % Clean up at our client, if netconf process was killed on node, 
    ct_netconfc:open(Name, [{timeout, 60000}]), % Then set up again.
    delete_mo_no_check(Name, ?NC_SessionNameList),
    throw({?MODULE, found});
nc_open({ok,_}, Name) ->
    %% nc_delete_rbsunit_no_check(Name),
    delete_mo_no_check(Name, ?NC_SessionNameList),
    throw({?MODULE, found});
nc_open(_, _) ->
    ok.

cli_connect({error,already_connected}, Name) ->
    rct_cli:disconnect(Name, noprint), % Clean up at our client, if cli process was killed on node, 
    rct_cli:connect(Name, noprint), % Then set up again.
    cli_delete_mo_no_check(Name, ?CLI_SessionNameList, noprint),
    throw({?MODULE, found});
cli_connect(ok, Name) ->
    cli_delete_mo_no_check(Name, ?CLI_SessionNameList, noprint),
    throw({?MODULE, found});
cli_connect(_, _) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [
    ].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @doc
%% calls do_meas_mem_operation_imm_oi with numbers of Hours that will perform the actual test. <br/>
%% LogPath where all the measured data will be stored. <br/>
%% CheckFlag = check | no_check , differents check will be done or not. <br/>
%% OperType = nc | cli | {nc, cli} , what type of opeartion is used. <br/>
%% @spec meas_mem_nc_operation_imm_oi_1_hour(Config) -> ok
%% @end
%%--------------------------------------------------------------------
%% 
%%%%%%%%%%%%%%%% NC %%%%%%%%%%%%%%%%%%%%
meas_mem_nc_operation_imm_oi_1_hour(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation_imm_oi(1, LogPath, check, nc, Config).

meas_mem_nc_operation_imm_oi_1_hour_backgroundload(Config) ->
    generate_cpu_load("50"),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation_imm_oi(1, LogPath, check, nc, Config).

meas_mem_nc_operation_imm_oi_10_hour(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation_imm_oi(10, LogPath, check, nc, Config).

meas_mem_nc_operation_imm_oi_50_hour(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation_imm_oi(50, LogPath, no_check, nc, Config).

meas_mem_nc_operation_imm_oi_150_hour(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation_imm_oi(150, LogPath, no_check, nc, Config).

meas_mem_nc_operation_imm_oi_150_hour_backgroundload(Config) ->
    generate_cpu_load("50"),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation_imm_oi(150, LogPath, no_check, nc, Config).

%%%%%%%%%%%%%%%% CLI %%%%%%%%%%%%%%%%%%%%
meas_mem_cli_operation_imm_oi_1_hour(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation_imm_oi(1, LogPath, check, cli, Config).

meas_mem_cli_operation_imm_oi_10_hour(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation_imm_oi(10, LogPath, check, cli, Config).

meas_mem_cli_operation_imm_oi_50_hour(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation_imm_oi(50, LogPath, no_check, cli, Config).

meas_mem_cli_operation_imm_oi_150_hour_backgroundload(Config) ->
    generate_cpu_load("50"),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation_imm_oi(150, LogPath, no_check, cli, Config).


%%%%%%%%%%%%%%%% NC & CLI %%%%%%%%%%%%%%%%%%%%
meas_mem_nc_cli_operation_imm_oi_1_hour(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation_imm_oi(1, LogPath, check, {nc, cli}, Config).

meas_mem_nc_cli_operation_imm_oi_10_hour(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation_imm_oi(10, LogPath, check, {nc, cli}, Config).

meas_mem_nc_cli_operation_imm_oi_50_hour(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation_imm_oi(50, LogPath, no_check, {nc, cli}, Config).

meas_mem_nc_cli_operation_imm_oi_150_hour_backgroundload(Config) ->
    generate_cpu_load("50"),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation_imm_oi(150, LogPath, no_check, {nc, cli}, Config).

%%%%%%%%%%%%%%%% NC & CLI Nr of repetitions %%%%%%%%%%%%%%%%%%%%
meas_mem_50_nc_operation_imm_oi(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_nrofrep_operation_imm_oi(50, LogPath, check, nc, Config).

meas_mem_50_cli_operation_imm_oi(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_nrofrep_operation_imm_oi(50, LogPath, check, cli, Config).

meas_mem_50_nc_cli_operation_imm_oi(Config) ->
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_nrofrep_operation_imm_oi(50, LogPath, check, {nc, cli}, Config).

%% ===========================================================================
%% @doc
%% This will do netconf operations and measure used memory size. <br/>
%% Fragment shall be owened by an application <br/>
%% The measured data will be stored in file. <br/>
%% Hours = integer() <br/>
%% LogPath = Path to ct logs directory. <br/>
%% CheckFlag = check | no_check <br/>
%% OperType = nc | cli | {nc, cli} <br/>
%% @spec do_meas_mem_operation_imm_oi(Hours, LogPath, CheckFlag, OperType, Config) -> ok
%% @end
%% ===========================================================================
do_meas_mem_operation_imm_oi(Hours, LogPath, CheckFlag, OperType, Config) ->

    %%%%
    %% Get sw version, using cli
    %%%%
    CXS_label = get_sw_version(),

    Seconds = Hours*60*60,

    [{_, NodeName}] = ct:get_config(test_nodes),

    BoardType = 
	proplists:get_value(board_type,ct:get_config(ct:get_config({test_nodes,1}))),
    ct:pal("BoardType: ~p .",[BoardType]),


    %%%%
    %% Get Pids. Will be used when get memsize.
    %%%%
    {BeamPid, TestNodeBeamPid} = get_pids(),

    %%%%
    %% Check OI exist, check for ImplementerOne,
    %%%%
    check_oi_exist(),
    
    %%%%
    %% Perform operations once.
    %%%%
    ct:pal("### Perform operation first time before get memory size,", []),
    NC_SubList = lists:sublist(?NC_SessionNameList,2),
    CLI_SubList = lists:sublist(?CLI_SessionNameList,2),
    case OperType of
	nc ->
	    case CheckFlag of
		check ->
		    ok = perform_nc_operations_once_check(?NC_SessionNameList);
		no_check ->
		    ok = perform_nc_operations_once_no_check(?NC_SessionNameList)
	    end;
	cli ->
	    case CheckFlag of
		check ->
		    ok = perform_cli_operations_once_check(?CLI_SessionNameList);
		no_check ->
		    ok = perform_cli_operations_once_no_check(?CLI_SessionNameList)
	    end;
	{nc, cli} ->
	    case CheckFlag of
		check ->
		    ok = perform_nc_cli_operations_once_check(NC_SubList, 
							      CLI_SubList);
		no_check ->
		    ok = perform_nc_cli_operations_once_no_check(NC_SubList, 
								 CLI_SubList)
	    end
    end,
    
    Branch = rct_tlib:get_branch(),
    case Branch of
	"R2A" ->
	    ct:pal("#LogPath_1# ~p", [LogPath++?All_OPERATION_IMM_OI_MEASURED_FILE_1]),
	    ct:pal("#LogPath_2# ~p", [LogPath++?All_OPERATION_IMM_OI_MEASURED_FILE_2]);
	_ ->
	    ct:pal("#~s_LogPath_1# ~p", [Branch, LogPath++?All_OPERATION_IMM_OI_MEASURED_FILE_1]),
	    ct:pal("#~s_LogPath_2# ~p", [Branch, LogPath++?All_OPERATION_IMM_OI_MEASURED_FILE_2])
    end,
    
    %%%%
    %% get memory size
    %%%%
    [Start_Tot_bc, 
     Start_Com_rss, 
     Start_Beam_rss,
     Start_TestBeam_rss] = get_used_mem_size([tot_bc, com, beam, test_beam], 
					     BeamPid,
					     TestNodeBeamPid),
    
    
    StartTime = os:timestamp(),
    {End_Tot_bc, 
     End_Com_rss, 
     End_Beam_rss, 
     End_TestBeam_rss, 
     Nr} = loop_operations(StartTime, 
			   Seconds,
			   %% 120,
			   Start_Tot_bc,
			   Start_Com_rss, 
			   Start_Beam_rss,
			   Start_TestBeam_rss,
			   LogPath,
			   BeamPid,
			   TestNodeBeamPid,		
			   CheckFlag,
			   OperType,
			   BoardType),
    
    %%%%
    %% Write info to file
    %%%%
    FileNameStr = 
	"meas_memory_nc_operation_imm_oi_"++ 
	integer_to_list(Hours) ++
	"_hour",
    FileName = rct_tlib:
	get_filename(FileNameStr),
    rct_tlib:writeDataToFile(?LOG_DIR, FileName, "~p;~w;~p;~s;~s;~p;~s;~s;~p;~s;~s;~p;~w~n", 
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
			      NodeName
			     ]),
    %%%%
    %% Print top
    %%%%
    Top = rct_rpc:call(rpc, os, cmd, ["top -b -n 1"], 10000, noprint),
    TOP = string:tokens(Top, "\n"),
    ct:pal("Nr: ~p, ~p",[Nr, TOP]),
    
    ct:pal("##LogDir1: ~p ",[?LOG_DIR++FileName]),
    
    ct:pal(" Start : Tot_bc: ~s,  com: ~s,  beam: ~s,  test_beam: ~s, ~n", [Start_Tot_bc,
									    Start_Com_rss,
									    Start_Beam_rss,
									    Start_TestBeam_rss
									   ]),
    
    ct:pal("End  : Tot_bc: ~s,  com: ~s,  beam: ~s,  test_beam: ~s, ~n", [End_Tot_bc,
									  End_Com_rss,
									  End_Beam_rss,
									  End_TestBeam_rss
									 ]),
    
    case CheckFlag of
	check ->
	    evaluate_mem_size(Start_Com_rss, End_Com_rss, Start_Beam_rss, End_Beam_rss);
	no_check ->
	    ok
    end,

    %%%%
    %% Delete OI for the testfragment
    %%%%
    Handle = proplists:get_value(handle, Config),
    ct:pal("¤¤ Handle End: ~p", [Handle]),
    ok = rct_safe_imm_oi_rpc:class_implementer_release(Handle, ?IMM_TEST_CLASS1),
    ct:pal("Clear Implementer ~p", [?IMPL_NAME_1]),
    ok = rct_safe_imm_oi_rpc:implementer_clear(Handle),
    ct:pal("Finalize OI Handle ~p", [Handle]),
    ok = rct_safe_imm_oi_rpc:finalize(Handle),

    %%%%
    %% check nc sessions deleted
    %%%%
    ct:pal("Check nc session and cli connections is deleted.",[]),
    [] = rct_rpc:call(rpc, os, cmd, ["pgrep netconf"], 10000, noprint),
    [] = rct_rpc:call(rpc, os, cmd, ["pgrep cli"], 10000, noprint),

    ct:pal("All measured memory is loged at: ~n "
	   "LogPath: ~p, ~n"
	   "filename 1:  ~p ~n "
	   "filename 2:  ~p ~n ",[LogPath, ?All_OPERATION_IMM_OI_MEASURED_FILE_1,
				  ?All_OPERATION_IMM_OI_MEASURED_FILE_2]),

    ok.


%% ===========================================================================
%% @doc
%% loop. <br/>
%% @end
%% ===========================================================================
loop_operations(StartTime, Duration, Start_Tot_bc, Start_Com_rss, Start_Beam_rss,Start_TestBeam_rss, LogPath, BeamPid, TestNodeBeamPid, CheckFlag, OperType, BoardType) ->
    loop_operations(StartTime, Duration, 0, Start_Tot_bc, Start_Com_rss, Start_Beam_rss, Start_TestBeam_rss, LogPath, BeamPid, TestNodeBeamPid, CheckFlag, OperType, BoardType).

loop_operations(StartTime, Duration, Nr, Start_Tot_bc, Start_Com_rss, Start_Beam_rss, Start_TestBeam_rss, LogPath, BeamPid, TestNodeBeamPid, CheckFlag, OperType, BoardType) ->
    ct:log("¤¤¤ loop nr: ~p", [Nr]),

    StartTimeStamp = os:timestamp(),
    NC_SubList = lists:sublist(?NC_SessionNameList,2),
    CLI_SubList = lists:sublist(?CLI_SessionNameList,2),
    case OperType of
	nc ->
	    case CheckFlag of
		check ->
		    ok = perform_nc_operations_once_check(?NC_SessionNameList);
		no_check ->
		    ok = perform_nc_operations_once_no_check(?NC_SessionNameList)
	    end;
	cli ->
	    case CheckFlag of
		check ->
		    ok = perform_cli_operations_once_check(?CLI_SessionNameList);
		no_check ->
		    ok = perform_cli_operations_once_no_check(?CLI_SessionNameList)
	    end;
	{nc, cli} ->
	    case CheckFlag of
		check ->
		    ok = perform_nc_cli_operations_once_check(NC_SubList,
							      CLI_SubList);
		no_check ->
		    ok = perform_nc_cli_operations_once_no_check(NC_SubList,
								 CLI_SubList)
	    end
    end,

    EndTimeStamp = os:timestamp(),
    TimeNC_Operation = trunc(timer:now_diff(EndTimeStamp, StartTimeStamp) / 1000 / 1000),
    case  TimeNC_Operation > 3600 of %% 1 hour
	true ->
	    ct:pal("Nc opeartion took more than one hour: ~p",[TimeNC_Operation]);
	false ->
	    ok
    end,

    %%%%
    %%get used memory size
    %%%%
    [End_Tot_bc, 
     End_Com_rss, 
     End_Beam_rss,
     End_TestBeam_rss] = get_used_mem_size([tot_bc, com, beam, test_beam], 
					   BeamPid,
					   TestNodeBeamPid
					  ),
    %% case Nr rem 10 of
    case Nr rem 100 of
    	0 ->
    	    Top = rct_rpc:call(rpc, os, cmd, ["top -b -n 1"], 10000, noprint),
    	    TOP = string:tokens(Top, "\n"),
    	    ct:pal("Nr: ~p, ~p",[Nr, TOP]);
    	_ ->
    	    ok
    end,

    %% case Nr rem 10 of
    case Nr rem 50 of
	0 ->
	    TimeStamp = os:timestamp(),
	    PassedTime = trunc(timer:now_diff(TimeStamp, StartTime) / 1000 / 1000),

	    ct:log("~p; Nr: ~p; Time: ~p; "
		   "Start_tot_bc: ~s; End_tot_bc: ~s; diff_tot_bc: ~p ; "
		   "Start_com: ~s; End_com: ~s; diff_com: ~p ; "
		   "Start_beam: ~s; End_beam: ~s; diff_beam: ~p;  ~n", 
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
	    
	    %% Logs will be stored in test log path.
	    rct_tlib:writeDataToFile(LogPath, ?All_OPERATION_IMM_OI_MEASURED_FILE_1, "~p; Nr: ~p; Opeation time: ~p; "
				     "Start_tot_bc: ~s; End_tot_bc: ~s; diff_tot_bc: ~p; "
				     "Start_com: ~s; End_com: ~s; diff_com: ~p ; "
				     "Start_beam: ~s; End_beam: ~s; diff_beam: ~p ~n", 
				     [httpd_util:rfc1123_date(),
				      Nr,
				      TimeNC_Operation,
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
	    
	    UsedDisk = get_use_disc_procentage(BoardType),
	    rct_tlib:writeDataToFile(LogPath, ?All_OPERATION_IMM_OI_MEASURED_FILE_2, "~p; Nr: ~p; "
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
	    {End_Tot_bc, End_Com_rss, End_Beam_rss, End_TestBeam_rss, Nr};
	false ->
	   loop_operations(StartTime, Duration, Nr+1, Start_Tot_bc, Start_Com_rss, Start_Beam_rss, Start_TestBeam_rss, LogPath, BeamPid, TestNodeBeamPid, CheckFlag, OperType, BoardType)
    end.

%% ===========================================================================
%% @doc
%% This will do netconf operations and measure used memory size. <br/>
%% Fragment shall be owened by an application <br/>
%% The measured data will be stored in file. <br/>
%% NrOfRep = integer() <br/>
%% LogPath = Path to ct logs directory. <br/>
%% CheckFlag = check | no_check <br/>
%% OperType = nc | cli | {nc, cli} <br/>
%% @spec do_meas_mem_nrofrep_operation_imm_oi(NrOfRep, LogPath, CheckFlag, OperType, Config) -> ok
%% @end
%% ===========================================================================
do_meas_mem_nrofrep_operation_imm_oi(NrOfRep, LogPath, CheckFlag, OperType, Config) ->
    %%%%
    %% Get sw version, using cli
    %%%%
    CXS_label = get_sw_version(),

    [{_, NodeName}] = ct:get_config(test_nodes),

    BoardType = 
	proplists:get_value(board_type,ct:get_config(ct:get_config({test_nodes,1}))),
    ct:pal("BoardType: ~p .",[BoardType]),

    %%%%
    %% Get Beam Pid. Will be used when get memsize.
    %%%%
    {BeamPid, TestNodeBeamPid} = get_pids(),

    %%%%
    %% Check OI exist, check for ImplementerOne,
    %%%%
    check_oi_exist(),
    
    %%%%
    %% Perform operations once.
    %%%%
    ct:pal("### Perform operation first time before get memory size,", []),
    NC_SubList = lists:sublist(?NC_SessionNameList,2),
    CLI_SubList = lists:sublist(?CLI_SessionNameList,2),
    case OperType of
	nc ->
	    case CheckFlag of
		check ->
		    ok = perform_nc_operations_once_check(
			   ?NC_SessionNameList);
		no_check ->
		    ok = perform_nc_operations_once_no_check(
			   ?NC_SessionNameList)
	    end;
	cli ->
	    case CheckFlag of
		check ->
		    ok = perform_cli_operations_once_check(
			   ?CLI_SessionNameList);
		no_check ->
		    ok = perform_cli_operations_once_no_check(
			   ?CLI_SessionNameList)
	    end;
	{nc, cli} ->
	    case CheckFlag of
		check ->
		    ok = perform_nc_cli_operations_once_check(NC_SubList, 
							      CLI_SubList);
		no_check ->
		    ok = perform_nc_cli_operations_once_no_check(
			   NC_SubList, 
			   CLI_SubList)
	    end
    end,
    
    ct:pal("#LogPath_1# ~p", [LogPath++
				  ?All_OPERATION_IMM_OI_MEASURED_FILE_1]),
    ct:pal("#LogPath_2# ~p", [LogPath++
				  ?All_OPERATION_IMM_OI_MEASURED_FILE_2]),
    
    %%%%
    %% get memory size
    %%%%
    [Start_Tot_bc, 
     Start_Com_rss, 
     Start_Beam_rss,
     Start_TestBeam_rss] = get_used_mem_size([tot_bc, 
					      com, 
					      beam, 
					      test_beam], 
					     BeamPid,
					     TestNodeBeamPid),
    
    StartTime = os:timestamp(),
    
    {End_Tot_bc, 
     End_Com_rss, 
     End_Beam_rss, 
     End_TestBeam_rss} = loop_operations_nrofrep( 
			   NrOfRep,
			   %% 10,
			   Start_Tot_bc,
			   Start_Com_rss, 
			   Start_Beam_rss,
			   Start_TestBeam_rss,
			   LogPath,
			   BeamPid,
			   TestNodeBeamPid,		
			   CheckFlag,
			   OperType,
			   BoardType),
    
    EndTime = os:timestamp(),
    TotTime = trunc(timer:now_diff(EndTime, StartTime) / 1000 / 1000),
    ct:pal("TotTime: ~p sec.",[TotTime]),
    
    %%%%
    %% Write info to file
    %%%%
    FileNameStr = "meas_memory_"++
	integer_to_list(NrOfRep)++
	"_nc_operation_imm_oi",
    FileName = rct_tlib:
	get_filename(FileNameStr),
    rct_tlib:writeDataToFile(?LOG_DIR, 
			     FileName, 
			     "~p;~w;~p;~p;~s;~s;~p;~s;~s;~p;~s;~s;~p;~w~n", 
			     [httpd_util:rfc1123_date(),
			      CXS_label,
			      NrOfRep,
			      TotTime,
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
			      NodeName
			     ]),
    
    %%%%
    %% Print top
    %%%%
    Top = rct_rpc:call(rpc, os, cmd, ["top -b -n 1"], 10000, noprint),
    TOP = string:tokens(Top, "\n"),
    ct:pal("Nr Of Repetition done: ~p, ~p",[NrOfRep, TOP]),
    
    ct:pal(" Start : Tot_bc: ~s,  com: ~s,  beam: ~s,  test_beam: ~s, ~n", 
	   [Start_Tot_bc,
	    Start_Com_rss,
	    Start_Beam_rss,
	    Start_TestBeam_rss
	   ]),
    
    ct:pal("End  : Tot_bc: ~s,  com: ~s,  beam: ~s,  test_beam: ~s, "
	   "Tot_Time: ~p, ~n", 
	   [End_Tot_bc,
	    End_Com_rss,
	    End_Beam_rss,
	    End_TestBeam_rss,
	    TotTime
	   ]),
    
    ct:pal("##LogDir1: ~p ",[?LOG_DIR++FileName]),
    
    case CheckFlag of
	check ->
	    evaluate_mem_size(Start_Com_rss, 
			      End_Com_rss, 
			      Start_Beam_rss, 
			      End_Beam_rss);
	no_check ->
	    ok
    end,

    %%%%
    %% Delete OI for the testfragment
    %%%%
    Handle = proplists:get_value(handle, Config),
    ok = rct_safe_imm_oi_rpc:class_implementer_release(Handle, ?IMM_TEST_CLASS1),
    ct:pal("Clear Implementer ~p", [?IMPL_NAME_1]),
    ok = rct_safe_imm_oi_rpc:implementer_clear(Handle),
    ct:pal("Finalize OI Handle ~p", [Handle]),
    ok = rct_safe_imm_oi_rpc:finalize(Handle),

    %%%%
    %% check nc sessions deleted
    %%%%
    ct:pal("Check nc session and cli connections is deleted.",[]),
    [] = rct_rpc:call(rpc, os, cmd, ["pgrep netconf"], 10000, noprint),
    [] = rct_rpc:call(rpc, os, cmd, ["pgrep cli"], 10000, noprint),

    ct:pal("All measured memory is loged at: ~n "
	   "LogPath: ~p, ~n"
	   "filename 1:  ~p ~n "
	   "filename 2:  ~p ~n ",[LogPath, 
				  ?All_OPERATION_IMM_OI_MEASURED_FILE_1,
				  ?All_OPERATION_IMM_OI_MEASURED_FILE_2]),

    ok.

%% ===========================================================================
%% @doc
%% loop Nr of repetions. <br/>
%% @end
%% ===========================================================================
loop_operations_nrofrep(NrOfRep, Start_Tot_bc, Start_Com_rss, 
			Start_Beam_rss,Start_TestBeam_rss, LogPath, BeamPid, 
			TestNodeBeamPid, CheckFlag, OperType, BoardType) ->

    NC_SubList = lists:sublist(?NC_SessionNameList,2),
    CLI_SubList = lists:sublist(?CLI_SessionNameList,2),
    NrOfRepList = lists:seq(0, NrOfRep),

    lists:
	foreach(
	  fun(Nr)->
		  ct:pal("Nr: ~p", [Nr]),
		  case OperType of
		      nc ->
			  case CheckFlag of
			      check ->
				  ok = 
				      perform_nc_operations_once_check(
					?NC_SessionNameList);
			      no_check ->
				  ok = 
				      perform_nc_operations_once_no_check(
					?NC_SessionNameList)
			  end;
		      cli ->
			  case CheckFlag of
			      check ->
				  ok = 
				      perform_cli_operations_once_check(
					?CLI_SessionNameList);
			      no_check ->
				  ok = 
				      perform_cli_operations_once_no_check(
					?CLI_SessionNameList)
			  end;
		      {nc, cli} ->
			  case CheckFlag of
			      check ->
				  ok = 
				      perform_nc_cli_operations_once_check(
					NC_SubList,
					CLI_SubList);
			      no_check ->
				  ok = 
				      perform_nc_cli_operations_once_no_check(
					NC_SubList,
					CLI_SubList)
			  end
		  end,
			
                  %%%%
		  %% get used memory size
                  %%%%
		  [End_Tot_bc, 
		   End_Com_rss, 
		   End_Beam_rss,
		   End_TestBeam_rss] = get_used_mem_size([tot_bc, com, beam, 
							  test_beam], 
							 BeamPid,
							 TestNodeBeamPid
							),
		  case Nr rem 50 of
		      0 ->
			  Top = rct_rpc:call(rpc, os, cmd, ["top -b -n 1"], 10000, noprint),
			  TOP = string:tokens(Top, "\n"),
			  ct:pal("Nr: ~p, ~p",[Nr, TOP]);
		      _ ->
			  ok
		  end,
		  
		  %% case Nr rem 10 of
		  case Nr rem 50 of
		      0 ->
			  ct:log(
			    "~p; Nr: ~p; "
			    "Start_tot_bc: ~s; End_tot_bc: ~s; "
			    "diff_tot_bc: ~p ; "
			    "Start_com: ~s; End_com: ~s; "
			    "diff_com: ~p ; "
			    "Start_beam: ~s; End_beam: ~s; "
			    "diff_beam: ~p;  ~n", 
			    [httpd_util:rfc1123_date(),
			     Nr,
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
				 list_to_integer(Start_Beam_rss)
			    ]),
	    
			  %% Logs will be stored in test log path.
			  rct_tlib:
			      writeDataToFile(
				LogPath, 
				?All_OPERATION_IMM_OI_MEASURED_FILE_1, 
				"~p; Nr: ~p; "
				"Start_tot_bc: ~s; End_tot_bc: ~s; "
				"diff_tot_bc: ~p; "
				"Start_com: ~s; End_com: ~s; "
				"diff_com: ~p; "
				"Start_beam: ~s; End_beam: ~s; "
				"diff_beam: ~p ~n", 
				[httpd_util:rfc1123_date(),
				 Nr,
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
				     list_to_integer(Start_Beam_rss)
				]),
			  
			  UsedDisk = get_use_disc_procentage(BoardType),
			  rct_tlib:
			      writeDataToFile(LogPath, 
					      ?All_OPERATION_IMM_OI_MEASURED_FILE_2, 
					      "~p; Nr: ~p; "
					      "Start_TestBeam: ~s; "
					      "End_TestBeam: ~s; "
					      "diff_TestBeam: ~p; "
					      "UsedDisc: ~p ~n",
					      [httpd_util:rfc1123_date(),
					       Nr,
					       Start_TestBeam_rss,
					       End_TestBeam_rss,
					       list_to_integer(
						 End_TestBeam_rss) - 
						   list_to_integer(
						     Start_TestBeam_rss),
					       UsedDisk
					      ]);
		      _ ->
			  ok
		  end
	  end, NrOfRepList),
    
    %%%%
    %%get used memory size
    %%%%
    [End_Tot_bc, 
     End_Com_rss, 
     End_Beam_rss,
     End_TestBeam_rss] = get_used_mem_size([tot_bc, com, beam, test_beam], 
					   BeamPid,
					   TestNodeBeamPid),
    
    {End_Tot_bc, End_Com_rss, End_Beam_rss, End_TestBeam_rss}.


%% ===========================================================================
%% @doc
%% Create MO, Delete MO, check MOs is created and deleted. <br/>
%% @spec perform_nc_operations_once_check(NC_SessionNameList) -> ok
%% @end
%% ===========================================================================
perform_nc_operations_once_check(NC_SessionNameList) ->
    %%%%
    %% Create MO instances
    %%%%
    nc_create_mo_inst(NC_SessionNameList),

    %%%%
    %% Check MO instances added
    %%%%
    ok = nc_check_mo_instance_created(NC_SessionNameList),

    %%%%
    %% Delete MO instances
    %%%%
    nc_delete_mo_inst(NC_SessionNameList),

    %%%%
    %% Check MO instances deleted
    %%%%
    ok = nc_check_mo_instance_deleted(NC_SessionNameList),

    ok.

%% ===========================================================================
%% @doc
%% Create MO, Delete MO, no check. if something goes wrong then continue <br/>
%% @spec perform_nc_operations_once_no_check(NC_SessionNameList) -> ok
%% @end
%% ===========================================================================
perform_nc_operations_once_no_check(NC_SessionNameList) ->
    %%%%
    %% Create a MO instance.
    %%%%
    nc_add_mo_no_check(NC_SessionNameList),

    %%%%
    %% Delete MO instance
    %%%%
    nc_delete_mo_no_check(NC_SessionNameList),

    ok.

%% ===============
nc_add_mo_no_check(NC_SessionNameList) ->
    %%%%
    %% Create a MO instance.
    %%%%
    NrOfSessions = length(NC_SessionNameList),
    NrList = lists:seq(1, NrOfSessions),
    ToupleList = lists:zip(NC_SessionNameList, NrList),

    lists:foreach(fun({SessionName, Nr})->
			  %% ct:pal("### Open NC session, Name: ~p", [SessionName]),

			  case ct_netconfc:open(SessionName,[{timeout, 60000}]) of
			      {ok,_} ->
				  ok;
			      Ret ->
				  ct:pal("Fail to open NC session: ~p ,  ~p ",[SessionName, Ret])
			  end,

			  TestClassName = atom_to_list(SessionName),
			  AttrName = integer_to_list(Nr),
			  case rct_nc_testclass1_lib:nc_add_mo_instance_and_attr_no_check(SessionName, 
											  TestClassName, 
											  AttrName) of
			      ok ->
				  %%%%
				  %% Close netconf session.
                                  %%%%  
				  case ct_netconfc:close_session(SessionName, 60000) of
				      ok ->
					  ok;
				      Res ->
					  ct:pal("Fail to close session: ~p ,  ~p ",[SessionName, Res])
				  end;
			      Res1  ->
				  ct:pal("Fail to add RbsUnit: ~p ",[Res1])
			  end
		  end, ToupleList),
    
   case rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint) of
	[] ->
	    ok;
	Result1 ->
	    ct:pal("Fail to send sync via rpc: ~p.\n Check top ",[Result1]),
	    rct_rs232:login(console),
	    ct_telnet:cmd(console, "top -b -n 1", 30000)
    end,

    ok.

%% ===============
nc_delete_mo_no_check(NC_SessionNameList) ->
    %%%%
    %% Delete MO instance
    %%%%
    lists:foreach(fun(SessionName1)->
			  %% ct:pal("### Open NC session, Name: ~p", [SessionName]),
			  case  ct_netconfc:open(SessionName1,[{timeout, 60000}]) of
			      {ok,_} ->
				  ok;
			      Ret1 ->
				  ct:pal("Fail to open NC session: ~p,  ~p ",[SessionName1, Ret1])
			  end,

			  TestClassName1 = atom_to_list(SessionName1),
			  case rct_nc_testclass1_lib:nc_delete_mo_instance_no_check(SessionName1, 
										    TestClassName1) of
			      ok ->
				  %% Close netconf session.
				  case ct_netconfc:close_session(SessionName1, 60000) of
				      ok ->
					  ok;
				      Res2 ->
					  ct:pal("Fail to close session: ~p,  ~p ",[SessionName1, Res2])
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


%% ===========================================================================
%% @doc
%% Create MO, Delete MO, check MOs is created and deleted. <br/>
%% @spec perform_cli_operations_once_check(CLI_SessionNameList) -> ok
%% @end
%% ===========================================================================
perform_cli_operations_once_check(CLI_SessionNameList) ->
    %%%%
    %% Create MO instances
    %%%%
    cli_create_mo_inst(CLI_SessionNameList),

    %%%%
    %% Delete rbs units
    %%%%
    cli_delete_mo_inst(CLI_SessionNameList),

    ok.

%% ===========================================================================
%% @doc
%% Create MO, Delete MO, no check. if something goes wrong then continue <br/>
%% @spec perform_cli_operations_once_no_check(CLI_SessionNameList) -> ok
%% @end
%% ===========================================================================
perform_cli_operations_once_no_check(CLI_SessionNameList) ->
    PrintOpt = noprint,
    %%%%
    %% Create a MO instance.
    %%%%
    cli_add_mo_no_check(CLI_SessionNameList, PrintOpt),

    %%%%
    %% Delete MO instance
    %%%%
    cli_delete_mo_no_check(CLI_SessionNameList, PrintOpt),

    ok.
    
%% ===============
cli_add_mo_no_check(CLI_SessionNameList, PrintOpt) ->
    %%%%
    %% Create a MO instance.
    %%%%
    NrOfSessions = length(CLI_SessionNameList),
    NrList = lists:seq(1, NrOfSessions),
    ToupleList = lists:zip(CLI_SessionNameList, NrList),

    lists:foreach(fun({SessionName, Nr})->
			  case rct_cli:connect(SessionName, PrintOpt) of
			      ok ->
				  ok;
			      Ret ->
				  ct:pal("Fail to open CLI session:~p,  ~p ",[SessionName, Ret])
			  end,

			  TestClassName = atom_to_list(SessionName),
			  try
			      rct_cli:send(SessionName, "configure", PrintOpt),
			      rct_cli:send(SessionName, "ManagedElement=1,TestRoot=1,TestClass1="++TestClassName, PrintOpt), 
			      %% Add attribute.
			      rct_cli:send(SessionName, "int32="++integer_to_list(Nr), PrintOpt),
			      rct_cli:send(SessionName, "commit", PrintOpt),
			      rct_cli:send(SessionName, "top", PrintOpt)
			  catch
			      _:Result ->
				  ct:pal("Fail to create MO : ~p",[Result])
			  end,
			  
			  case rct_cli:disconnect(SessionName, PrintOpt) of
			      ok ->
				  ok;
			      Res ->
				  ct:pal("Fail to close cli session: ~p,  ~p ",[SessionName, Res])
			  end
		  end, ToupleList),
    
    case rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint) of
	[] ->
	    ok;
	Result1 ->
	    ct:pal("Fail to send sync via rpc: ~p.\n Check top ",[Result1]),
	    rct_rs232:login(console),
	    ct_telnet:cmd(console, "top -b -n 1", 30000)
    end,
    
    ok.

%% ===============
cli_delete_mo_no_check(CLI_SessionNameList, PrintOpt) ->
    %%%%
    %% Delete MO instance
    %%%%
    lists:foreach(fun(SessionName1)->
			  %% ct:pal("### Open NC session, Name: ~p", [SessionName]),
			  case rct_cli:connect(SessionName1, PrintOpt) of
			      ok ->
				  ok;
			      Ret1 ->
				  ct:pal("Fail to open CLI session: ~p,  ~p ",[SessionName1, Ret1])
			  end,

			  TestClassName1 = atom_to_list(SessionName1),
			  try
			      rct_cli:send(SessionName1, "configure", PrintOpt),
			      rct_cli:send(SessionName1, "ManagedElement=1,TestRoot=1", PrintOpt), 
			      rct_cli:send(SessionName1, "no TestClass1="++TestClassName1, PrintOpt), 
			      rct_cli:send(SessionName1, "commit", PrintOpt),
			      rct_cli:send(SessionName1, "top", PrintOpt)
			      catch
				  _:Result2 ->
				      ct:pal("Fail to delete MO : ~p",[Result2])
			      end,

			  case rct_cli:disconnect(SessionName1, PrintOpt) of
			      ok ->
				  ok;
			      Res2 ->
				  ct:pal("Fail to close cli session: ~p,  ~p ",[SessionName1, Res2])
			  end
		  end, CLI_SessionNameList),
    
    case rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint) of
	[] ->
	    ok;
	Result3 ->
	    ct:pal("Fail to send sync via rpc: ~p.\n Check top ",[Result3]),
	    rct_rs232:login(console),
	    ct_telnet:cmd(console, "top -b -n 1", 30000)
    end,
  
    ok.

%% ===========================================================================
%% @doc
%% Create MO, Delete MO, check MOs is created and deleted. <br/>
%% @spec perform_nc_cli_operations_once_check(NC_SessionNameList, CLI_SessionNameList) -> ok
%% @end
%% ===========================================================================
perform_nc_cli_operations_once_check(NC_SessionNameList,CLI_SessionNameList) ->
    %%%%
    %% Create MO instances
    %%%%
    nc_create_mo_inst(NC_SessionNameList),
    cli_create_mo_inst(CLI_SessionNameList),

    %%%%
    %% Check MO instances added
    %%%%
    ok = nc_check_mo_instance_created(NC_SessionNameList),

    %%%%
    %% Delete rbs units
    %%%%
    nc_delete_mo_inst(NC_SessionNameList),
    cli_delete_mo_inst(CLI_SessionNameList),

    %%%%
    %% Check MO instances deleted
    %%%%
    ok = nc_check_mo_instance_deleted(NC_SessionNameList),

    ok.

%% ===========================================================================
%% @doc
%% Create MO, Delete MO, no check. if something goes wrong then continue <br/>
%% @spec perform_nc_cli_operations_once_no_check(NC_SessionNameList, CLI_SessionNameList) -> ok
%% @end
%% ===========================================================================
perform_nc_cli_operations_once_no_check(NC_SessionNameList, CLI_SessionNameList) ->
    PrintOpt = noprint,
    %%%%
    %% Create a MO instance.
    %%%%
    nc_add_mo_no_check(NC_SessionNameList),
    cli_add_mo_no_check(CLI_SessionNameList, PrintOpt),

    %%%%
    %% Delete MO instance
    %%%%
    nc_delete_mo_no_check(NC_SessionNameList),
    cli_delete_mo_no_check(CLI_SessionNameList, PrintOpt),

    ok.

%% ===========================================================================
%% @doc
%% Create MO instance. <br/>
%% - Open sessions <br/>
%% - Create MO. <br/>
%% - Close sessions.
%% @spec nc_create_mo_inst(NC_SessionNameList) -> ok
%% @end
%% ===========================================================================
nc_create_mo_inst(NC_SessionNameList) ->
    %%%%
    %% Create a MO instance.
    %%%%
    NrOfSessions = length(NC_SessionNameList),
    NrList = lists:seq(1, NrOfSessions),
    ToupleList = lists:zip(NC_SessionNameList, NrList),

    lists:foreach(fun({SessionName, Nr})->
			  %% ct:pal("### Open NC session, Name: ~p", [SessionName]),
			  open_nc_session("nc_create_mo_inst", SessionName),

			  TestClassName = atom_to_list(SessionName),
			  AttrName = integer_to_list(Nr),
			  ok = rct_nc_testclass1_lib:nc_add_mo_instance_and_attr(SessionName, 
										 TestClassName, 
										 AttrName),
			  %%%%
			  %% Close netconf session.
                          %%%%    
			  close_nc_session("nc_create_mo_inst", SessionName)

		  end, ToupleList),
    
    rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint).

%% ===========================================================================
%% @doc
%% Open NC session. <br/>
%% - Action is from what function this is called from <br/>
%% @spec  open_nc_session(Action, SessionName)-> ok
%% @end
%% ===========================================================================
open_nc_session(Action, SessionName) ->
    open_nc_session(Action, SessionName, 60000).

open_nc_session(Action, SessionName, Timeout) when Timeout < 500 ->
    ct:pal("~p: ~p: TC will fail due to nc session could not be open within 1 min",[Action, SessionName]),
    ct:fail("TC will fail due to nc session could not be open");

open_nc_session(Action, SessionName, Timeout) ->
    case ct_netconfc:open(SessionName,[{timeout, 2000}]) of
	{ok,_} -> 
	    ok;
	{error,closed} ->
	    ct:log("~p: ~p: nc session could not be open: {error,closed}. "
		   "Try again after 5 sec.",[Action, SessionName]),
	    timer:sleep(5000),
	    open_nc_session(Action, SessionName, Timeout-5000);
	Error ->
	    ct:log("~p: ~p: nc session could not be open: ~p. Try again after 5 sec.",[Action, SessionName, Error]),
	    timer:sleep(3000),
	    open_nc_session(Action, SessionName, Timeout-5000)
    end.

%% ===========================================================================
%% @doc
%% Close NC session. <br/>
%% - Action is from what function this is called from <br/>
%% @spec  close_nc_session(Action, SessionName)-> ok
%% @end
%% ===========================================================================
close_nc_session(Action, SessionName) ->
    close_nc_session(Action, SessionName, 60000).

close_nc_session(Action, SessionName, Timeout) when Timeout < 500 ->
    ct:pal("~p: ~p: TC will fail due to nc session could not be Closed within 1 min",[Action, SessionName]),
    ct:fail("TC will fail due to nc session could not be Closed");

close_nc_session(Action, SessionName, Timeout) ->
    case ct_netconfc:close_session(SessionName, 2000) of
	ok -> 
	    ok;
	{error,{no_connection_found,_}} ->
	    ok;
	Error ->
	    ct:log("~p: ~p: session could not be closed: ~p. Try again after 5 sec",[Action, SessionName, Error]),
	    timer:sleep(5000),
	    close_nc_session(Action, SessionName, Timeout-5000)
    end.


%% ===========================================================================
%% @doc
%% CLI create MO instance. <br/>
%% - Open sessions <br/>
%% - Create MO. <br/>
%% - Close sessions.
%% @spec cli_create_mo_inst(CLI_SessionNameList) -> ok
%% @end
%% ===========================================================================
cli_create_mo_inst(CLI_SessionNameList) ->
    %%%%
    %% Create a MO instance.
    %%%%
    PrintOpt = noprint,
    NrOfSessions = length(CLI_SessionNameList),
    NrList = lists:seq(1, NrOfSessions),
    ToupleList = lists:zip(CLI_SessionNameList, NrList),
    lists:foreach(fun({SessionName, Nr}) ->
			  %% Open cli connection
			  ok = rct_cli:connect(SessionName, PrintOpt),

			  TestClassName = atom_to_list(SessionName),
			  rct_cli:send(SessionName, "configure", "\\(config\\)>", PrintOpt),
			  rct_cli:send(SessionName, "ManagedElement=1,TestRoot=1,TestClass1="++TestClassName, "\\(config-TestClass1="++TestClassName++"\\)>", PrintOpt), 
			  %% Add attribute.
			  rct_cli:send(SessionName, "int32="++integer_to_list(Nr), PrintOpt), 
			  rct_cli:send(SessionName, "commit", "\\(config-TestClass1="++TestClassName++"\\)>", PrintOpt),

			  {ok ,RecievedData} = rct_cli:send(SessionName, "show", PrintOpt), 
			  case re:run(RecievedData, TestClassName) of
			      {match, _} ->
				  ok;
			      nomatch -> 
				  ct:pal("TestClassName does not exist: ~p.",[TestClassName]),
				  ct:fail("Expected TestClassName does not exist")
			  end,
			  rct_cli:send(SessionName, "top", ">", PrintOpt),
			  %% Close cli connection
			  ok = rct_cli:disconnect(SessionName, PrintOpt)
		  end, 
		  ToupleList),

    rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint).

%% ===========================================================================
%% @doc
%% Delete MO instance. <br/>
%% - Open sessions <br/>
%% - Delete MO. <br/>
%% - Close sessions.
%% @spec nc_delete_mo_inst(NC_SessionNameList) -> ok
%% @end
%% ===========================================================================
nc_delete_mo_inst(NC_SessionNameList) ->
    %%%%
    %% Delete MO instances.
    %%%%
    lists:foreach(fun(SessionName)->
			  %% ct:pal("### Open NC session, Name: ~p", [SessionName]),
			  open_nc_session("nc_delete_mo_inst", SessionName),

			  TestClassName = atom_to_list(SessionName),
			  ok = rct_nc_testclass1_lib:nc_delete_mo_instance(SessionName, 
									   TestClassName),
			  %%%%
			  %% Close netconf session.
                          %%%%    
			  close_nc_session("nc_delete_mo_inst", SessionName)
		  end, NC_SessionNameList),
    
    rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint).

%% ===========================================================================
%% @doc
%% CLI Delete MO instance. <br/>
%% - Open sessions <br/>
%% - Delete MO. <br/>
%% - Close sessions.
%% @spec cli_delete_mo_inst(CLI_SessionNameList) -> ok
%% @end
%% ===========================================================================
cli_delete_mo_inst(CLI_SessionNameList) ->
    %%%%
    %% Delete MO instances.
    %%%%
    PrintOpt = noprint,
    lists:foreach(fun(SessionName) ->
			  %% Open cli connection
			  %% ok = rct_cli:connect(SessionName, PrintOpt),
			  cli_setup_sess(SessionName, PrintOpt),

			  TestClassName = atom_to_list(SessionName),
			  rct_cli:send(SessionName, "configure", "\\(config\\)>", PrintOpt),
			  rct_cli:send(SessionName, "ManagedElement=1,TestRoot=1", "\\(config-TestRoot=1\\)>", PrintOpt), 
			  %% Delete attribute.
			  rct_cli:send(SessionName, "no TestClass1="++TestClassName, "\\(config-TestRoot=1\\)>", PrintOpt), 
			  rct_cli:send(SessionName, "commit", "\\(config-TestClass1="++TestClassName++"\\)>", PrintOpt),
			  {ok ,RecievedData} = rct_cli:send(SessionName, "show", PrintOpt), 
			  RcvdDataList = string:tokens(RecievedData,"\r\n "),
			  cli_check_element_not_exist("TestClass1="++TestClassName, RcvdDataList),

			  rct_cli:send(SessionName, "top", ">", PrintOpt),
			  %% Close cli connection
			  %% ok = rct_cli:disconnect(SessionName, PrintOpt)
			  rct_cli:disconnect(SessionName, PrintOpt)
		  end, 
		 CLI_SessionNameList),

    %% [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint).
    rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint).


cli_setup_sess(SessionName, PrintOpt) ->
    cli_setup_sess(SessionName, PrintOpt, 60000).

cli_setup_sess(_SessionName, _PrintOpt, Timeout) when Timeout < 0 ->
    ct:fail("Could not setup cli session within max timeout!");
cli_setup_sess(SessionName, PrintOpt, Timeout) ->
    case rct_cli:connect(SessionName, PrintOpt) of
	ok ->
	    ok;
	_Other ->
	    ct:log("could not setup cli session. Sleep an try again."),
	    timer:sleep(5000),
	    cli_setup_sess(SessionName, PrintOpt, Timeout-5000)
    end.

%% ===========================================================================
%% @doc
%% Check MO instance created. <br/>
%% - Open sessions <br/>
%% - read MO one by one. <br/>
%% - Close sessions.
%% @spec nc_check_mo_instance_created(NC_SessionNameList) -> ok
%% @end
%% ===========================================================================
nc_check_mo_instance_created(NC_SessionNameList)->
    %%%%
    %% Open netconf session.
    %%%%
    lists:foreach(fun(SessionName)->
			  %% ct:pal("### Open NC session, Name: ~p", [SessionName]),
			  open_nc_session("nc_check_mo_instance_created", SessionName)
		  end, NC_SessionNameList),
    
			  

    lists:foreach(fun(SessionName)->
    			  TestClassName = atom_to_list(SessionName),
			  ok = rct_nc_testclass1_lib:nc_check_mo_instance_created(SessionName, 
										  TestClassName)
		  end, NC_SessionNameList),

			      
    %%%%
    %% Close netconf session.
    %%%%    
    lists:foreach(fun(SessionName1)->
			  close_nc_session("nc_check_mo_instance_created", SessionName1)
		  end, NC_SessionNameList),
    
    ok.

%% ===========================================================================
%% @doc
%% Check MO instance deleted. <br/>
%% - Open sessions <br/>
%% - read MO one by one. <br/>
%% - Close sessions.
%% @spec nc_check_mo_instance_deleted(NC_SessionNameList) -> ok
%% @end
%% ===========================================================================
nc_check_mo_instance_deleted(NC_SessionNameList)->
    %%%%
    %% Open netconf session.
    %%%%
    lists:foreach(fun(SessionName)->
			  %% ct:pal("### Open NC session, Name: ~p", [SessionName]),
			  open_nc_session("nc_check_mo_instance_deleted", SessionName)
		  end, NC_SessionNameList),
    
    lists:foreach(fun(SessionName)->
			  TestClassName = atom_to_list(SessionName),
			  ok = rct_nc_testclass1_lib:nc_check_mo_instance_deleted(SessionName, 
										  TestClassName)
		  end, NC_SessionNameList),

    %%%%
    %% Close netconf session.
    %%%%    
    lists:foreach(fun(SessionName1)->			  
			  close_nc_session("nc_check_mo_instance_deleted", SessionName1)
		  end, NC_SessionNameList),
    
    ok.


%%--------------------------------------------------------------------
%% @doc 
%% get_used_mem_size. <br/>
%% @end
%%--------------------------------------------------------------------
get_used_mem_size(WantedSizes, BeamPid, TestNodeBeamPid) ->
    MemSizes = lists:map(fun(X) ->
				 case X of
				     tot_bc ->
					 %% Free = rct_rpc:call(rpc, os, cmd, ["free"], 10000, noprint),
					 case rct_rpc:call(rpc, os, cmd, ["free"], 10000, noprint) of
					     {badrpc,timeout} ->
						 ct:log("A timout on os:cmd",[]);
						 %% Tot_bc = "dummy";
					     Free ->
						 {match,[Tot_bc]} = re:run(Free,"buffers/cache: *([0-9]+).*",[{capture,[1],list}]),
						 Tot_bc
					 end;
				     com ->
					 %% Com_Rss = rct_rpc:call(rpc, os, cmd, ["ps -eo fname,rss | egrep '(com)'"], 10000, noprint),
					 case rct_rpc:call(rpc, os, cmd, ["ps -eo fname,rss | egrep '(com)'"], 10000, noprint) of
					     {badrpc,timeout} ->
						 ct:log("B timout on os:cmd",[]);
					     Com_Rss ->
						 ["com", Com_rss] = string:tokens(Com_Rss," \n"),
						 Com_rss
					 end;
				     beam ->
					 %% Beam_Rss_Data = rct_rpc:call(rpc, os, cmd, ["ps -o fname,rss -p "++BeamPid], 10000, noprint),
					 case rct_rpc:call(rpc, os, cmd, ["ps -o fname,rss -p "++BeamPid], 10000, noprint) of
					     {badrpc,timeout} ->
						 ct:log("C timout on os:cmd",[]);
					     Beam_Rss_Data ->
						 ["COMMAND","RSS","beam.smp", Beam_rss] = string:tokens(Beam_Rss_Data," \n"),
						 Beam_rss
					 end;
				     test_beam ->
					 %% Test_Beam_Rss_Data = rct_rpc:call(rpc, os, cmd, ["ps -o fname,rss -p "++TestNodeBeamPid], 10000, noprint),
					 case rct_rpc:call(rpc, os, cmd, ["ps -o fname,rss -p "++TestNodeBeamPid], 10000, noprint) of
					     {badrpc,timeout} ->
						 ct:log("D timout on os:cmd",[]);
					     Test_Beam_Rss_Data ->
						 ["COMMAND","RSS","beam.smp", Test_Beam_rss] = string:tokens(Test_Beam_Rss_Data," \n"),
						 Test_Beam_rss
					 end
				     end
			 end, WantedSizes),
    %% ct:pal("MemSizes: ~p ",[MemSizes]),
    MemSizes.


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


%% Used by cleanup if tc fail.
%% ===========================================================================
%% @doc
%% Delete MO no check. Used in cleanup if TC fail<br/>
%% - Delete MO. <br/>
%% @spec delete_mo_no_check(SessionName, NrList) -> ok
%% @end
%% ===========================================================================
delete_mo_no_check(SessionName, NC_SessionNameList)->
    lists:foreach(fun(Name)->
			  ct_netconfc:open(SessionName,[{timeout, 60000}]),
			  TestClassName = atom_to_list(Name),
			  ok = rct_nc_testclass1_lib:nc_delete_mo_instance_no_check(SessionName, 
										    TestClassName),
			  ct_netconfc:close_session(SessionName, 60000)

		  end, NC_SessionNameList).

%% ===========================================================================
%% @doc
%% Delete MO no check. Used in cleanup if TC fail<br/>
%% - Delete MO. <br/>
%% @spec cli_delete_mo_no_check(SessionName, CLI_SessionNameList, PrintOpt) -> ok
%% @end
%% ===========================================================================
cli_delete_mo_no_check(SessionName, CLI_SessionNameList, PrintOpt) ->
    lists:foreach(fun(Name) ->
			  TestClassName = atom_to_list(Name),
			  
			  rct_cli:send(SessionName, "configure", PrintOpt),
			  rct_cli:send(SessionName, "ManagedElement=1,TestRoot=1", PrintOpt), 
			  rct_cli:send(SessionName, "no TestClass1="++TestClassName, PrintOpt)
			  %% rct_cli:send(SessionName, "commit", PrintOpt)
		  end, 
		  CLI_SessionNameList),
    timer:sleep(5000),
    rct_cli:send(SessionName, "commit", PrintOpt),
    rct_cli:send(SessionName, "top", PrintOpt).

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% reboot the node.  <br/>
%% %% @spec reboot_node() -> ok
%% %% @end
%% %%--------------------------------------------------------------------
%% reboot_node() ->
%%     %%%%
%%     %% reboot
%%     %%%%   
%%     {ok, TestNode} = rct_safe_imm_oi_rpc:get_testnode(rct_safe_imm_oi_rpc),
%%     rct_safe_imm_oi_rpc:subscribe_testnode(rct_safe_imm_oi_rpc),

%%     ct:pal("### reboot!",[]),
%%     {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
%%     ok = rct_rs232:login(console),
%%     %% net_kernel:disconnect(TestNode),
%%     %% net_kernel:disconnect(ErlNode),
%%     ok = ct_telnet:send(console, "reboot"),
%%     net_kernel:disconnect(TestNode),
%%     net_kernel:disconnect(ErlNode),
%%     timer:sleep(5000),
%%     ok = wait_for_testnode(node_down),
%%     {ok,_} = ct_telnet:expect(console, "login:", [{timeout,60000}, no_prompt_check]),
%%     net_kernel:connect(ErlNode),

%%     %%%%
%%     %% Wait for testnode up.
%%     %%%%
%%     ok = wait_for_testnode(node_up),
%%     net_kernel:connect(TestNode),

%%     %%%%
%%     %% Sleep to be sure that com is up and ready to be used.
%%     %%%%
%%     timer:sleep(50000).


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
					  ct:fail(" Background load is lower than expected! ")
				  end
    			  end
    		  end, CpuLoad),
    ok.


%% ===========================================================================
%% @doc
%% Get CXS label. <br/>
%% Get SW version using COM cli interface. <br/>
%% @spec get_sw_version() -> 'CXS_LABEL'
%% @end
%% ===========================================================================
get_sw_version() ->
    ct:pal("### Get SW version",[]),
    [CLI_Name|_T] = ?CLI_SessionNameList,
    CXS_label = rct_tlib:get_sw_version(CLI_Name),
    CXS_label.

%% %%-------------------------------------------------------------------- 
%% %% @hidden
%% %%-------------------------------------------------------------------- 
%% wait_for_testnode(NodeState)->
%%     wait_for_testnode(NodeState, rct_safe_imm_oi_rpc).
%% wait_for_testnode(NodeState, InstanceName) ->	   
%%     receive
%% 	{InstanceName, {NodeName, NodeState}} ->
%% 	    ct:pal("NodeName: ~p , NodeState: ~p .",[NodeName, NodeState]),
%% 	    ok
%%     after 120000 ->
%% 	    ct:fail({"Expected NodeState not rceived",NodeState})
%%     end.


%%-------------------------------------------------------------------- 
%% @hidden
%%-------------------------------------------------------------------- 
cli_check_element_not_exist(_Element, []) ->
    ok;
cli_check_element_not_exist(Element, [H | T]) ->
    case ( H == Element ) of
        true  -> 
	    ct:pal("TestClassName was not deleted: ~p.",[Element]),
	    ct:fail("TestClassName was not deleted. ",[]);
        false -> 
	    cli_check_element_not_exist(Element, T)
    end.

%%-------------------------------------------------------------------- 
%% @hidden
%%-------------------------------------------------------------------- 
get_use_disc_procentage(BoardType) ->
    DF =  rct_rpc:call(rpc, os, cmd, ["df"], 10000, 
		       noprint),
    Df = string:tokens(DF, " \n"),
    %% ct:log("### Df: ~p", [Df]),
    DiskNr = case BoardType of  %% Get element nr in list.
		 BoardType when BoardType == "dus4101";
				%% BoardType == "dus5201"; %% Remove this when RCSEE is used
				BoardType == "duw4101" ->
		     ct:log("### BoardType: ~p, check discusage in /disc", [BoardType]),
		     string:str(Df, ["/disk"]);
		 _ ->
		     ct:log("### BoardType: ~p, check discusage in /home/sirpa", [BoardType]),
		     string:str(Df, ["/home/sirpa"])  % ARM, mnesia is written here.
	     end,
    %% ct:log("### DiskNr: ~p", [DiskNr]),
    UsedDisk = lists:nth(DiskNr-1, Df),
    %% ct:log("### UsedDisk: ~p", [UsedDisk]),
    UsedDisk.
    

%%-------------------------------------------------------------------- 
%% @hidden
%%-------------------------------------------------------------------- 
check_oi_exist() ->
    case rct_rpc:call(rpc, safs_imm_db, ci_get, ['TESTMOMTestClass1'], 
		      10000, noprint) of
	{_,<<"ImplementerOne">>} ->
	    ok;
	{_,<<"ImplementerOne">>, _} ->
	    ok;
	Ans ->
	    ct:pal("~p", [Ans]),
	    ct:fail(" Expecting OI does not exist! ")
    end.

%%-------------------------------------------------------------------- 
%% @hidden
%%-------------------------------------------------------------------- 
get_pids() ->
    %%%%
    %% Get Beam Pid. Will be used when get memsize.
    %%%%
    BeamPid = rct_rpc:call(rpc, os, getpid, [], 10000),
    ct:pal("BeamPid: ~p .",[BeamPid]),

    %%%%
    %% Get testnode Beam Pids.
    %%%%
    %% get_testnode_beam_pid(),
    {ok, TestNode} = rct_safe_imm_oi_rpc:get_testnode(rct_safe_imm_oi_rpc),
    TestNodeBeamPid = rpc:call(TestNode, os, getpid, [], 5000),
    ct:pal("TestNodeBeamPid: ~p .",[TestNodeBeamPid]),

    ComPid = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 10000, noprint),
    ct:pal("### Com: ~p",[ComPid]),

    {BeamPid, TestNodeBeamPid}.

%%-------------------------------------------------------------------- 
%% @hidden
%%-------------------------------------------------------------------- 
wait_for_testnode_up() ->
    wait_for_testnode_up(120000).
wait_for_testnode_up(Timeout) when Timeout < 0 ->
    ct:fail("TC will fail due to Testnode is not up within 2 min.");
wait_for_testnode_up(Timeout) ->
    case rct_safe_imm_oi_rpc:get_testnode(rct_safe_imm_oi_rpc) of
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

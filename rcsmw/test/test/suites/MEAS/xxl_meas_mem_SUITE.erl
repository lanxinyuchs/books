%%% #0.    BASIC INFORMATION
%% coding: latin-1
%%% ----------------------------------------------------------
%%% %CCaseFile:	xxl_meas_mem_SUITE.erl %
%%% @author erarube
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R9A/3
%%% 
%%% @doc == Measure memory usages when netconf users operates.==
%%% This Test Suite can be used on target enviroment.
%%% Used sw version and measured data is written to a file.
%%% Path: /proj/rcs/measurements/
%%%
%%% <br/><br/>
%%% 
%%% @end

-module(xxl_meas_mem_SUITE).
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
%%% R2A/2      2013-11-26 etxivri     Created
%%% R2A/3      2013-11-29 etxivri     Added HW type on File name, 
%%%                                   to separate the measured files.
%%% R2A/4      2013-11-29 etxivri     Use of rct_tlib to add HW type on FileName
%%% R2A/5      2013-12-02 etxivri     Update str when writetofile.
%%% R2A/6      2014-01-16 etxkols     Support for dus5201. 
%%% R2A/7      2014-03-07 etxivri     Update to add Branch file name when 
%%%                                   other than Branch R2A is used.
%%% R2A/8      2014-06-04 etxivri     Changed check for "du1 login" prompt to 
%%%                                   "login:"
%%% R2A/9      2014-07-08 etxivri     Update due to limit of nr off ssh session
%%%                                    is set to 5 at same time.
%%% R3A/1      2014-12-01 etxivri     Use SwInventory when get sw version.
%%% R3A/2      2015-14-13 etxivri     Decrease cpu load check.
%%%                                   Changed used disc space check.
%%% R3A/3      2015-14-13 etxivri     Changed used disc space check.
%%% R4A/1      2015-10-06 etxmlar     Allow 2 CPU:s to have lower background load
%%%                                   than expected.
%%% R4A/2      2015-10-06 etxmlar     Added printout
%%% R4A/3      2015-10-08 etxivri     Update to use silent mode in cth_conn_log
%%% R4A/4      2015-10-16 etxivri     Remove ct:pal due ti it fill html log.
%%% R4A/5      2015-11-27 etxivri     Minor update using silentmode i hook.
%%% R4A/6      2016-02-19 etxkols     Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
%%% R9A/1      2017-03-20 erarube     Added support for c608, dus33, dus53,dus6303, dus6502
%%% R9A/2      2017-03-20 erarube     Added missing data for new boards on CatPidList
%%% R9A/3      2017-11-13 erarube     Support for dus5201tk and dus3201tk
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
	 nc_operation_1_hour/1,
	 nc_operation_10_hour_no_check/1,
	 nc_operation_150_hour_no_check/1,
	 nc_operation_150_hour_no_check_bgl/1,
	 %% Cli
	 cli_operation_1_hour/1,

	 cli_operation_150_hour_no_check_bgl/1,
	 %% Netconf and Cli
	 nc_cli_operation_1_hour/1,
	 nc_cli_operation_10_hour_no_check/1,
	 nc_cli_operation_150_hour_no_check/1,
	 nc_cli_operation_150_hour_no_check_bgl/1
	]).


%% -define(CLI_Name, cli1). %% CLI hook name

-define(NrOfNetConfUsers, 5).
%% -define(NrOfNetConfUsers, 20).
-define(NC_SessionNameList, 
	[list_to_atom(
	   "nc"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfNetConfUsers)]).

-define(NrOfCliUsers, 5).
%% -define(NrOfCliUsers, 20).
-define(CLI_SessionNameList, 
	[list_to_atom(
	   "cli"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfCliUsers)]).

-define(All_OPERATION_IMM_OI_MEASURED_FILE_1, 
	"all_operation_imm_oi_measured_memory_file_1.txt").
-define(All_OPERATION_IMM_OI_MEASURED_FILE_2, 
	"all_operation_imm_oi_measured_memory_file_2.txt").

-define(LOG_DIR, "/proj/rcs/measurements/xxl/").
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
    NetconfHooks = 
	[{rct_netconf, 
	  list_to_atom(
	    "nc"++integer_to_list(N))} || N <- lists:seq(1,?NrOfNetConfUsers)],
    CliHooks = 
	[{rct_cli, 
	  {list_to_atom(
	     "cli"++integer_to_list(N)), 
	   [manual_connect]}} || N <- lists:seq(1,?NrOfCliUsers)],

    [{timetrap, {hours, 200}},
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 %% %% {cth_conn_log, []},
		 {rct_consserv,cs1},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
					       []}}]}},
                 {rct_rs232,console},
		 {cth_conn_log,[{ct_netconfc,[{log_type,silent}]} ]},
		 %% {cth_conn_log, [{ct_netconfc,[{log_type,silent},
		 %% 		  {hosts,[NetconfHooks]}]} ]},
		 {rct_cli, {cli1, [manual_connect]}},
		 %% %% {rct_safe_imm_oi_rpc,[{safe_debug_level, 0}, 
		 %% %% 		       {finalize_on_fail, true}]},
		 %% %% {rct_safe_imm_oi_rpc,[ {targ_du_no, 1},
		 %% %% 			{instance_name, ?INSTANCE_NAME},
		 %% %% 			{finalize_on_fail, true}]},
		 %% %% {rct_safe_rpc,[{safe_services, [{imm, 2}]},
		 %% %% 		{instance_name, rct_safe_imm_oi_rpc}, 
		 %% %% 		{finalize_on_fail, true}]},
		 {rct_safe_rpc,[{safe_services, 
				 [{imm, 0}]}, %% debuglevel 0
				{instance_name, rct_safe_imm_oi_rpc}, 
				{finalize_on_fail, true}]},
		 {rct_tlib,{identifier,[]}},
		 {rct_scp, [{1, node1}]},
                 %% %% {rct_core,[]} | NetconfHooks
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
    Config.


%% @hidden
end_per_testcase(_TestCase, Config) ->
    ct:pal("End per TC.", []),

    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p.  \nClean up added instances", 
		   [Reason]),

	    A = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 
			     10000, noprint),
	    B = rct_rpc:call(rpc, os, cmd, ["pgrep cli"], 10000, noprint),
	    C = rct_rpc:call(rpc, os, cmd, ["pgrep netconf"], 10000, noprint),
	    D = rct_rpc:call(rpc, os, getpid, [], 10000),

	    ct:pal("### Com: ~p",[A]),
	    ct:pal("### Cli: ~p",[B]),
	    ct:pal("### Netconf: ~p",[C]),
	    ct:pal("### BeamPid: ~p .",[D]),


	    OI = rct_rpc:call(rpc, safs_imm_db, ci_get, ['TESTMOMTestClass1'], 
			      10000, noprint),
	    ct:pal("~p.", [OI]),

	    %%%%%%%%%%%%%%
	    %% Clean up Netconf configurations!
	    %%%%%%%%%%%%%%
	    %%%%
	    %% Open a netconf session and clean up instances.
            %%%%
	    try
	    	lists:foreach(
		  fun(Name) -> 
			  nc_open(ct_netconfc:open(Name, 
						   [{timeout, 60000}]), Name) 
		  end, ?NC_SessionNameList)
	    catch throw:{?MODULE, found} ->
	    	    ok
	    end,

            %%%%
	    %% Close nc sessions
            %%%%
	    lists:foreach(fun(Name1) ->
	    			  %% ct:pal("### Cleanup after fail TC, 
				  %% Close session: ~p", [Name1]),
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
	    	lists:foreach(
		  fun(Name2) -> cli_connect(rct_cli:connect(Name2, noprint), 
					    Name2) end, ?CLI_SessionNameList)
	    catch throw:{?MODULE, found} ->
	    	    ok
	    end,
	    
	    %%%%
	    %% Close cli connection
	    %%%%
	    lists:foreach(fun(Name3) ->
	    			  %% ct:pal("### Cleanup after fail TC, 
				  %% CloseName: ~p", [Name3]),
	    			  rct_cli:disconnect(Name3, noprint)
	    		  end, 
	    		  ?CLI_SessionNameList)
	    

    end,

    %%%%
    %% Clean up cpulimit, don't care if it has been used or not.!
    %% When start cpulimit with -z flag, then Only need to delete target 
    %% process "cat".
    %%%%
    %% rct_rpc:call(rpc, os, cmd, ["pkill cpulimit"], 10000, noprint),
    %% timer:sleep(2000),
    rct_rpc:call(rpc, os, cmd, ["pkill cat"], 10000, noprint),
    rct_rpc:call(rpc, os, cmd, ["rm "++?CPULIMIT_TO_PATH++"cpulimit"], 10000, 
		 noprint),

    %% Sleep to make sure that processes are finished
    timer:sleep(10000),
    [] = rct_rpc:call(rpc, os, cmd, ["pgrep cat"], 10000, noprint),
    [] = rct_rpc:call(rpc, os, cmd, ["pgrep cpulimit"], 10000, noprint),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    ok.


nc_open({error,_}, Name) ->
    %% Clean up at our client, if netconf process was killed on node, 
    ct_netconfc:close_session(Name, 30000), 
    ct_netconfc:open(Name, [{timeout, 60000}]), % Then set up again.
    delete_mo_no_check(Name, ?NC_SessionNameList),
    throw({?MODULE, found});
nc_open({ok,_}, Name) ->
    delete_mo_no_check(Name, ?NC_SessionNameList),
    throw({?MODULE, found});
nc_open(_, _) ->
    ok.

cli_connect({error,already_connected}, Name) ->
    %% Clean up at our client, if cli process was killed on node, 
    rct_cli:disconnect(Name, noprint), 
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
%% calls do_meas_mem_operation with numbers of Hours that will <br/>
%% perform the actual test. <br/>
%% LogPath where all the measured data will be stored. <br/>
%% CheckFlag = check | no_check , differents check will be done or not. <br/>
%% OperType = nc | cli | {nc, cli} , what type of opeartion is used. <br/>
%% @spec nc_operation_1_hour(Config) -> ok
%% @end
%%--------------------------------------------------------------------
%% 
%%%%%%%%%%%%%%%% NC %%%%%%%%%%%%%%%%%%%%
nc_operation_1_hour(Config) ->
    reboot_node(),
    generate_cpu_load("50"),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation(1, LogPath, check, nc).

nc_operation_10_hour_no_check(Config) ->
    reboot_node(),
    generate_cpu_load("50"),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation(10, LogPath, no_check, nc).

nc_operation_150_hour_no_check(Config) ->
    reboot_node(),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation(150, LogPath, no_check, nc).

nc_operation_150_hour_no_check_bgl(Config) ->
    reboot_node(),
    generate_cpu_load("50"),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation(150, LogPath, no_check, nc).


%%%%%%%%%%%%%%%% CLI %%%%%%%%%%%%%%%%%%%%
cli_operation_1_hour(Config) ->
    reboot_node(),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation(1, LogPath, check, cli).

cli_operation_150_hour_no_check_bgl(Config) ->
    %% reboot_node(),
    generate_cpu_load("50"),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation(150, LogPath, no_check, cli).


%%%%%%%%%%%%%%%% NC & CLI %%%%%%%%%%%%%%%%%%%%
nc_cli_operation_1_hour(Config) ->
    reboot_node(),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation(1, LogPath, check, {nc, cli}).

nc_cli_operation_10_hour_no_check(Config) ->
    reboot_node(),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation(10, LogPath, no_check, {nc, cli}).

nc_cli_operation_150_hour_no_check(Config) ->
    reboot_node(),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation(150, LogPath, no_check, {nc, cli}).

nc_cli_operation_150_hour_no_check_bgl(Config) ->
    reboot_node(),
    generate_cpu_load("50"),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_operation(150, LogPath, no_check, {nc, cli}).

%% ===========================================================================
%% @doc
%% This will do netconf operations and measure used memory size. <br/>
%% Fragment shall be owened by an application <br/>
%% The measured data will be stored in file. <br/>
%% Hours = integer() <br/>
%% LogPath = Path to ct logs directory. <br/>
%% CheckFlag = check | no_check <br/>
%% OperType = nc | cli | {nc, cli} <br/>
%% @spec do_meas_mem_operation(Hours, LogPath, CheckFlag, OperType) -> ok
%% @end
%% ===========================================================================
do_meas_mem_operation(Hours, LogPath, CheckFlag, OperType) ->
    %%%%
    %% Get sw version, using cli
    %%%%
    CXS_label = get_sw_version(),

    Seconds = Hours*60*60,

    [{_, NodeName}] = ct:get_config(test_nodes),

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
    TestNodeBeamPid = rpc:call(TestNode, os, getpid, [], 10000),
    ct:pal("TestNodeBeamPid: ~p .",[TestNodeBeamPid]),

    ComPid = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 
			  10000, noprint),
    ct:pal("### Com: ~p",[ComPid]),

    %%%%
    %% Create a Class Implementer.
    %%%%

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

    try
	check_oi_exist(),

        %%%%
	%% Perform operations once.
        %%%%
	ct:pal("### Perform operation first time before get memory size,", []),
	NC_SubList = lists:sublist(?NC_SessionNameList,10),
	CLI_SubList = lists:sublist(?CLI_SessionNameList,10),
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
			ok = 
			    perform_nc_cli_operations_once_check(NC_SubList, 
								 CLI_SubList);
		    no_check ->
			ok = 
			    perform_nc_cli_operations_once_no_check(NC_SubList, 
								    CLI_SubList)
		end
	end,

	Branch = rct_tlib:get_branch(),
	case Branch of
	    "R2A" ->
		ct:pal("#LogPath_1# ~p", 
		       [LogPath++?All_OPERATION_IMM_OI_MEASURED_FILE_1]),
		ct:pal("#LogPath_2# ~p", 
		       [LogPath++?All_OPERATION_IMM_OI_MEASURED_FILE_2]);
	    _ ->
		ct:pal("#~s_LogPath_1# ~p",
		       [Branch, LogPath++?All_OPERATION_IMM_OI_MEASURED_FILE_1]),
		ct:pal("#~s_LogPath_2# ~p", 
		       [Branch, LogPath++?All_OPERATION_IMM_OI_MEASURED_FILE_2])
	end,

        %%%%
	%% get memory size
        %%%%
	[Start_Tot_bc, 
	 Start_Com_rss, 
	 Start_Beam_rss,
	 Start_TestBeam_rss,
	 Start_test_oi,
	 Start_ift_app,
	 Start_test_app] =  get_used_mem_size(["tot_bc", 
					       "com", 
					       "beam", 
					       "test_beam", 
					       "test_oi", 
					       "ift_app", 
					       "test_app"], 
					      BeamPid, 
					      TestNodeBeamPid),
	

	StartTime = os:timestamp(),

	Nr = loop_operations(StartTime, 
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
			     CheckFlag,
			     OperType),
        %%%%
	%%get used memory size
        %%%%
	[End_Tot_bc,
	 End_Com_rss, 
	 End_Beam_rss,
	 End_TestBeam_rss,
	 End_test_oi,
	 End_ift_app,
	 End_test_app] = get_used_mem_size(["tot_bc", 
						"com", 
						"beam", 
						"test_beam", 
						"test_oi", 
						"ift_app", 
						"test_app"], 
					       BeamPid, 
					       TestNodeBeamPid),

        %%%%
	%% Write info to file
        %%%%
	%% FileName = "meas_memory_nc_operation_imm_oi_"++
	%%     integer_to_list(Hours) ++"_hour",
	FileNameOperType = case OperType of
			       nc -> "nc";
			       cli -> "cli";
			       {nc, cli} -> "nc_cli"
			   end,
	FileNameCheckFlag = case CheckFlag of
				check -> "check";
				_ -> "no_check"
			    end,
	FileNameStr = FileNameOperType++
	    "_operation_"++
	    integer_to_list(Hours)++
	    "_hour_"++
	    FileNameCheckFlag,
	FileName = rct_tlib:
	    get_filename(FileNameStr),
	rct_tlib:writeDataToFile(?LOG_DIR, FileName, 
				 "~p;~w;~p;~s;~s;~p;~s;~s;~p;~s;"
				 "~s;~p;~s;~s;~p;~s;~s;~p;~s;~s;"
				 "~p;~s;~s;~p~w~n", 
				 [httpd_util:rfc1123_date(),
				  CXS_label,
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
				      list_to_integer(Start_Beam_rss),
				  Start_TestBeam_rss,
				  End_TestBeam_rss,
				  list_to_integer(End_TestBeam_rss) - 
				      list_to_integer(Start_TestBeam_rss),
				  Start_test_oi,
				  End_test_oi,
				  list_to_integer(End_test_oi) - 
				      list_to_integer(Start_test_oi),
				  Start_ift_app,
				  End_test_oi,
				  list_to_integer(End_test_oi) - 
				      list_to_integer(Start_ift_app),
				  Start_test_app,
				  End_test_app,
				  list_to_integer(End_test_app) - 
				      list_to_integer(Start_test_app),
				  NodeName
				 ]),
	%%%%
	%% Print top
        %%%%
	Top = rct_rpc:call(rpc, os, cmd, ["top -b -n 1"], 10000, noprint),
	TOP = string:tokens(Top, "\n"),
	ct:pal("Nr: ~p, ~p",[Nr, TOP]),

	ct:pal("##LogDir1: ~p ",[?LOG_DIR++FileName]),

	ct:pal("Start: com: ~s,  beam: ~s,  TestBeam: ~s,  test_oi: ~s,  "
	       "ift_app: ~s,  test_app: ~s ~n", [Start_Com_rss,
						 Start_Beam_rss,
						 Start_TestBeam_rss,
						 Start_test_oi,
						 Start_ift_app,
						 Start_test_app
						]),
	
	ct:pal("End  : com: ~s,  beam: ~s,  TestBeam: ~s,  test_oi: ~s,  "
	       "ift_app: ~s,  test_app: ~s ~n", [End_Com_rss,
						 End_Beam_rss,
						 End_TestBeam_rss,
						 End_test_oi,
						 End_ift_app,
						 End_test_app
						]),


	
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
	ct:pal("Release Class Implementer for: ~p", [?IMM_TEST_CLASS1]),
	ok = rct_safe_imm_oi_rpc:class_implementer_release(Handle, 
							   ?IMM_TEST_CLASS1),
	ct:pal("Clear Implementer ~p", [?IMPL_NAME_1]),
	ok = rct_safe_imm_oi_rpc:implementer_clear(Handle),
	ct:pal("Finalize OI Handle ~p", [Handle]),
	ok = rct_safe_imm_oi_rpc:finalize(Handle)
	    
    catch
	_:Reason -> %% Case TC fail, then delete OI.
            %%%%
	    %% Delete OI for the testfragment
            %%%%
	    ct:pal("Tc will fail. Reason:~p",[Reason]),
	    ct:pal("Release Class Implementer for: ~p", [?IMM_TEST_CLASS1]),
	    ok = 
		rct_safe_imm_oi_rpc:class_implementer_release(Handle, 
							      ?IMM_TEST_CLASS1),
	    ct:pal("Clear Implementer ~p", [?IMPL_NAME_1]),
	    ok = rct_safe_imm_oi_rpc:implementer_clear(Handle),
	    ct:pal("Finalize OI Handle ~p", [Handle]),
	    ok = rct_safe_imm_oi_rpc:finalize(Handle),

	    ct:fail(Reason)
    end,
    
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
%% loop. <br/>
%% @end
%% ===========================================================================
loop_operations(StartTime, Duration, Start_Tot_bc, Start_Com_rss, 
		Start_Beam_rss,Start_TestBeam_rss, Start_test_oi,
		Start_ift_app, Start_test_app, LogPath, BeamPid, 
		TestNodeBeamPid, CheckFlag, OperType) ->
    loop_operations(StartTime, Duration, 0, Start_Tot_bc, Start_Com_rss, 
		    Start_Beam_rss, Start_TestBeam_rss, Start_test_oi,
		    Start_ift_app, Start_test_app, LogPath, BeamPid, 
		    TestNodeBeamPid, CheckFlag, OperType).

loop_operations(StartTime, Duration, Nr, Start_Tot_bc, Start_Com_rss,
		Start_Beam_rss, Start_TestBeam_rss, Start_test_oi,
		Start_ift_app, Start_test_app, LogPath, BeamPid, 
		TestNodeBeamPid, CheckFlag, OperType) ->
    %% ct:pal("¤¤¤ Perform NC imm oi, nr: ~p", [Nr]),

    %% StartTimeStamp = os:timestamp(),

    NC_SubList = lists:sublist(?NC_SessionNameList,10),
    CLI_SubList = lists:sublist(?CLI_SessionNameList,10),
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
		    ok = perform_nc_cli_operations_once_no_check(NC_SubList,
								 CLI_SubList)
	    end
    end,

    %% EndTimeStamp = os:timestamp(),
    %% TimeNC_Operation = 
    %% 	trunc(timer:now_diff(EndTimeStamp, StartTimeStamp) / 1000 / 1000),
    %% case  TimeNC_Operation > 3600 of %% 1 hour
    %% 	true ->
    %% 	    ct:pal("Nc opeartion took more than one hour: ~p",
    %% 		   [TimeNC_Operation]);
    %% 	false ->
    %% 	    ok
    %% end,
    
    case Nr rem 200 of
    	0 ->
    	    Top = rct_rpc:call(rpc, os, cmd, ["top -b -n 1"], 10000, noprint),
    	    TOP = string:tokens(Top, "\n"),
    	    ct:pal("Nr: ~p, ~p",[Nr, TOP]),
	    CpuLimitPids = get_pid("cpulimit"),
	    ct:log("CpuLimitPids: ~p .",[CpuLimitPids]),
	    case CpuLimitPids of
		[] ->
		    ok; %% Do nothing.
		_ ->
		    CpuLimitMemUsage = 
			get_pid_mem_usage(CpuLimitPids, "cpulimit"),
		    ct:log("CpuLimitMemUsage: ~p .",[CpuLimitMemUsage])
	    end;
    	_ ->
    	    ok
    end,

    %% case Nr rem 10 of
    case Nr rem 50 of
	0 ->    
	    [End_Tot_bc, 
	     End_Com_rss, 
	     End_Beam_rss,
	     End_TestBeam_rss,
	     End_test_oi,
	     End_ift_app,
	     End_test_app] = get_used_mem_size(["tot_bc", 
						"com", 
						"beam", 
						"test_beam", 
						"test_oi", 
						"ift_app", 
						"test_app"], 
					       BeamPid, 
					       TestNodeBeamPid),
	    
 	    GetNrOfProc = rct_rpc:call(rpc, os, cmd, ["ps -A | wc"], 
				       10000, noprint),
	    [NrOfProc | _T] = string:tokens(GetNrOfProc, " \n"),

	    B = rct_rpc:call(rpc, ets, all, [], 10000, noprint),
	    BB = length(B),
	    ct:log("Nr of ets element: ~p",[BB]),

	    TimeStamp = os:timestamp(),
	    PassedTime = 
		trunc(timer:now_diff(TimeStamp, StartTime) / 1000 / 1000),

	    %% Needed for Jockes plot.
	    ct:log("~p; Nr: ~p; Time: ~p; "
		   "Start_tot_bc: ~s; End_tot_bc: ~s; diff_tot_bc: ~p ; "
		   "Start_com: ~s; End_com: ~s; diff_com: ~p ; "
		   "Start_beam: ~s; End_beam: ~s; diff_beam: ~p;  ~n", 
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
			list_to_integer(Start_Beam_rss)
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
		    Start_TestBeam_rss,
		    End_TestBeam_rss,
		    list_to_integer(End_TestBeam_rss) - 
			list_to_integer(Start_TestBeam_rss),
		    Start_test_oi,
		    End_test_oi,
		    list_to_integer(End_test_oi) - 
			list_to_integer(Start_test_oi),
		    Start_ift_app,
		    End_ift_app,
		    list_to_integer(End_ift_app) - 
			list_to_integer(Start_ift_app),
		    Start_test_app,
		    End_test_app,
		    list_to_integer(End_test_app) - 
			list_to_integer(Start_test_app)
		   ]),

	    %% Logs will be stored in test log path.
	    rct_tlib:writeDataToFile(LogPath, 
				     ?All_OPERATION_IMM_OI_MEASURED_FILE_1, 
				     "~p; Nr: ~p; "
				     "Opeation_time: ~p; "
				     "Start_tot_bc: ~s; End_tot_bc: ~s; "
				     "diff_tot_bc: ~p; "
				     "Start_com: ~s; End_com: ~s; "
				     "diff_com: ~p ; "
				     "Start_beam: ~s; End_beam: ~s; "
				     "diff_beam: ~p ~n", 
				     [httpd_util:rfc1123_date(),
				      Nr,
				      PassedTime,
				      %%TimeNC_Operation,
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
	    
	    DF = rct_rpc:call(rpc, os, cmd, ["df"], 10000, noprint),
	    Df = string:tokens(DF, " \n"),
	    ct:log("### Df: ~p", [Df]),
	    DiskNr = string:str(Df, ["/home/sirpa"]),
	    %% ct:pal("DiskNr: ~p", [DiskNr]),
	    UsedDisk = lists:nth(DiskNr-1, Df),
	    %% ct:pal("UsedDisk: ~p", [UsedDisk]),
	    rct_tlib:writeDataToFile(LogPath, 
				     ?All_OPERATION_IMM_OI_MEASURED_FILE_2, 
				     "~p; Nr: ~p; "
				     "Start_TestBeam: ~s; End_TestBeam: ~s; "
				     "diff_TestBeam: ~p; "
				     "Start_test_oi: ~s; End_test_oi: ~s; "
				     "diff_test_oi: ~p ; "
				     "Start_ift_app: ~s; End_ift_app: ~s; "
				     "diff_ift_app: ~p ; "
				     "Start_test_app: ~s; End_test_app: ~s; "
				     "diff_test_app: ~p "
				     "UsedDisc: ~p ~n",
	    			     [httpd_util:rfc1123_date(),
	    			      Nr,
				      Start_TestBeam_rss,
				      End_TestBeam_rss,
				      list_to_integer(End_TestBeam_rss) - 
					  list_to_integer(Start_TestBeam_rss),
				      Start_test_oi,
				      End_test_oi,
				      list_to_integer(End_test_oi) - 
					  list_to_integer(Start_test_oi),
				      Start_ift_app,
				      End_ift_app,
				      list_to_integer(End_ift_app) - 
					  list_to_integer(Start_ift_app),
				      Start_test_app,
				      End_test_app,
				      list_to_integer(End_test_app) - 
					  list_to_integer(Start_test_app),
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
	    Nr;
	false ->
	   loop_operations(StartTime, Duration, Nr+1, Start_Tot_bc, 
			   Start_Com_rss, Start_Beam_rss, Start_TestBeam_rss,
			   Start_test_oi, Start_ift_app, Start_test_app,
			   LogPath, BeamPid, TestNodeBeamPid, CheckFlag, 
			   OperType)
    end.

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
			  ct_netconfc:open(SessionName,
					   [{timeout, 60000}]) ,
			  TestClassName = atom_to_list(SessionName),
			  AttrName = integer_to_list(Nr),
			  rct_nc_testclass1_lib:
			      nc_add_mo_instance_and_attr_no_check(
				SessionName, TestClassName, AttrName),
			  ct_netconfc:close_session(SessionName, 
						    60000) 
		  end, ToupleList),
    
    rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),
    ok.

%% ===============
nc_delete_mo_no_check(NC_SessionNameList) ->
    %%%%
    %% Delete MO instance
    %%%%
    lists:foreach(fun(SessionName1)->
			  ct_netconfc:open(SessionName1,
					   [{timeout, 60000}]),

			  TestClassName1 = atom_to_list(SessionName1),
			  rct_nc_testclass1_lib:
			      nc_delete_mo_instance_no_check(SessionName1, 
							     TestClassName1),
			  
			  ct_netconfc:close_session(SessionName1, 
						    60000)
		  end, NC_SessionNameList),
    
    rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

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
				  ct:pal("Fail to open CLI session:~p,  ~p ",
					 [SessionName, Ret])
			  end,

			  TestClassName = atom_to_list(SessionName),
			  try
			      rct_cli:send(SessionName, "configure", PrintOpt),
			      rct_cli:send(SessionName, "ManagedElement=1,"
					   "TestRoot=1,TestClass1="++
					       TestClassName, PrintOpt), 
			      %% Add attribute.
			      rct_cli:send(SessionName, 
					   "int32="++integer_to_list(Nr), 
					   PrintOpt),
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
				  ct:pal("Fail to close cli session: ~p,  ~p ",
					 [SessionName, Res])
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
			  %% ct:pal("### Open NC session, Name: ~p", 
			  %% [SessionName]),
			  case rct_cli:connect(SessionName1, PrintOpt) of
			      ok ->
				  ok;
			      Ret1 ->
				  ct:pal("Fail to open CLI session: ~p,  ~p ",
					 [SessionName1, Ret1])
			  end,

			  TestClassName1 = atom_to_list(SessionName1),
			  try
			      rct_cli:send(SessionName1, "configure", PrintOpt),
			      rct_cli:send(SessionName1, 
					   "ManagedElement=1,TestRoot=1", 
					   PrintOpt), 
			      rct_cli:send(SessionName1, 
					   "no TestClass1="++TestClassName1, 
					   PrintOpt), 
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
				  ct:pal("Fail to close cli session: ~p,  ~p ",
					 [SessionName1, Res2])
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
perform_nc_cli_operations_once_no_check(NC_SessionNameList, 
					CLI_SessionNameList) ->
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
			  %% ct:pal("### Open NC session, Name: ~p", 
			  %% [SessionName]),
			  open_nc_session("nc_create_mo_inst", SessionName),

			  TestClassName = atom_to_list(SessionName),
			  AttrName = integer_to_list(Nr),
			  ok = rct_nc_testclass1_lib:
			      nc_add_mo_instance_and_attr(SessionName, 
							  TestClassName, 
							  AttrName),
			  %%%%
			  %% Close netconf session.
                          %%%%    
			  close_nc_session("nc_create_mo_inst", SessionName)

		  end, ToupleList),
    
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint).


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
    ct:pal("~p: ~p: TC will fail due to nc session could not be open "
	   "within 1 min",[Action, SessionName]),
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
	    ct:log("~p: ~p: nc session could not be open: ~p. "
		   "Try again after 5 sec.",[Action, SessionName, Error]),
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
    ct:pal("~p: ~p: TC will fail due to nc session could not be Closed "
	   "within 1 min",[Action, SessionName]),
    ct:fail("TC will fail due to nc session could not be Closed");

close_nc_session(Action, SessionName, Timeout) ->
    case ct_netconfc:close_session(SessionName, 2000) of
	ok -> 
	    ok;
	{error,{no_connection_found,_}} ->
	    ok;
	Error ->
	    ct:log("~p: ~p: session could not be closed: ~p. "
		   "Try again after 5 sec",[Action, SessionName, Error]),
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
			  rct_cli:send(SessionName, 
				       "configure", "\\(config\\)>", PrintOpt),
			  rct_cli:send(SessionName, 
				       "ManagedElement=1,TestRoot=1,"
				       "TestClass1="++TestClassName, 
				       "\\(config-TestClass1="++
					   TestClassName++"\\)>", PrintOpt), 
			  %% Add attribute.
			  rct_cli:send(SessionName, 
				       "int32="++integer_to_list(Nr), 
				       PrintOpt), 
			  rct_cli:send(SessionName, "commit", 
				       "\\(config-TestClass1="++
					   TestClassName++"\\)>", PrintOpt),

			  {ok ,RecievedData} = rct_cli:send(SessionName, 
							    "show", PrintOpt), 
			  case re:run(RecievedData, TestClassName) of
			      {match, _} ->
				  ok;
			      nomatch -> 
				  ct:pal("TestClassName does not exist: ~p.",
					 [TestClassName]),
				  ct:fail("Expected TestClassName does not "
					  "exist")
			  end,
			  rct_cli:send(SessionName, "top", ">", PrintOpt),
			  %% Close cli connection
			  ok = rct_cli:disconnect(SessionName, PrintOpt)
		  end, 
		  ToupleList),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint).

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
			  %% ct:pal("### Open NC session, Name: ~p", 
			  %% [SessionName]),
			  open_nc_session("nc_delete_mo_inst", SessionName),

			  TestClassName = atom_to_list(SessionName),
			  ok = rct_nc_testclass1_lib:
			      nc_delete_mo_instance(SessionName, 
						    TestClassName),
			  %%%%
			  %% Close netconf session.
                          %%%%    
			  close_nc_session("nc_delete_mo_inst", SessionName)
		  end, NC_SessionNameList),
    
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint).

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
			  ok = rct_cli:connect(SessionName, PrintOpt),

			  TestClassName = atom_to_list(SessionName),
			  rct_cli:send(SessionName, 
				       "configure", "\\(config\\)>", PrintOpt),
			  rct_cli:send(SessionName, 
				       "ManagedElement=1,TestRoot=1", 
				       "\\(config-TestRoot=1\\)>", PrintOpt), 
			  %% Delete attribute.
			  rct_cli:send(SessionName, 
				       "no TestClass1="++TestClassName, 
				       "\\(config-TestRoot=1\\)>", PrintOpt), 
			  rct_cli:send(SessionName, 
				       "commit", "\\(config-TestClass1="++
					   TestClassName++"\\)>", PrintOpt),
			  {ok ,RecievedData} = rct_cli:send(SessionName, 
							    "show", PrintOpt), 
			  RcvdDataList = string:tokens(RecievedData,"\r\n "),
			  cli_check_element_not_exist("TestClass1="++
							  TestClassName, 
						      RcvdDataList),

			  rct_cli:send(SessionName, "top", ">", PrintOpt),
			  %% Close cli connection
			  ok = rct_cli:disconnect(SessionName, PrintOpt)
		  end, 
		 CLI_SessionNameList),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint).

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
			  %% ct:pal("### Open NC session, Name: ~p", 
			  %% [SessionName]),
			  open_nc_session("nc_check_mo_instance_created", 
					  SessionName)
		  end, NC_SessionNameList),
    
			  

    lists:foreach(fun(SessionName)->
    			  TestClassName = atom_to_list(SessionName),
			  ok = rct_nc_testclass1_lib:
			      nc_check_mo_instance_created(SessionName, 
							   TestClassName)
		  end, NC_SessionNameList),

			      
    %%%%
    %% Close netconf session.
    %%%%    
    lists:foreach(fun(SessionName1)->
			  close_nc_session("nc_check_mo_instance_created", 
					   SessionName1)
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
			  %% ct:pal("### Open NC session, Name: ~p", 
			  %% [SessionName]),
			  open_nc_session("nc_check_mo_instance_deleted", 
					  SessionName)
		  end, NC_SessionNameList),
    
    lists:foreach(fun(SessionName)->
			  TestClassName = atom_to_list(SessionName),
			  ok = rct_nc_testclass1_lib:
			      nc_check_mo_instance_deleted(SessionName, 
							   TestClassName)
		  end, NC_SessionNameList),

    %%%%
    %% Close netconf session.
    %%%%    
    lists:foreach(fun(SessionName1)->			  
			  close_nc_session("nc_check_mo_instance_deleted", 
					   SessionName1)
		  end, NC_SessionNameList),
    
    ok.


%%--------------------------------------------------------------------
%% @doc 
%% get_used_mem_size. <br/>
%% @end
%%--------------------------------------------------------------------
get_used_mem_size(WantedSizes, BeamPid, TestNodeBeamPid) ->
    MemSizes = 
	lists:map(
	  fun(X) ->
		  case X of
		      "tot_bc" ->
			  Free = rct_rpc:call(rpc, os, cmd, ["free"], 10000, noprint),
			  {match,[Tot_bc]} = 
			      re:run(Free,".*available\nMem: *([0-9]+).*",
				     [{capture,[1],list}]),
			  Tot_bc;
		      "beam" ->
			  Beam_Rss_Data = 
			      rct_rpc:call(rpc, os, cmd, 
					   ["ps -o fname,rss -p "++BeamPid], 
					   10000, noprint),
			  ["COMMAND","RSS","beam.smp", Beam_rss] = 
			      string:tokens(Beam_Rss_Data," \n"),
			  Beam_rss;
		      "test_beam" ->
			  Test_Beam_Rss_Data = 
			      rct_rpc:call(rpc, os, cmd, 
					   ["ps -o fname,rss -p "++
						TestNodeBeamPid], 10000, 
					   noprint),
			  ["COMMAND","RSS",
			   "beam.smp", 
			   TestBeam_rss] = string:tokens(Test_Beam_Rss_Data,
							 " \n"),
			  TestBeam_rss;
		      ProcName ->
			  Rss_Data = 
			      rct_rpc:call(rpc, os, cmd, 
					   ["ps -eo fname,rss | egrep "++
						ProcName], 10000, noprint),
			  case ProcName of
			      "test_oi" ->
				  ["test_oi.", ProcName_rss] = 
				      string:tokens(Rss_Data," \n"),
				  ProcName_rss;
			      _ ->
				  [ProcName, ProcName_rss] = 
				      string:tokens(Rss_Data," \n"),
				  ProcName_rss
			  end
		  end
	  end, WantedSizes),
    %% ct:pal("MemSizes: ~p ",[MemSizes]),
    MemSizes.

get_pid_mem_usage(PidList, ProcName)->
        MemSizes = lists:map(fun(Pid) ->
				     Rss_Data = 
					 rct_rpc:call(rpc, os, cmd, 
						      ["ps -o fname,rss -p "++
							   Pid], 10000, noprint),
				     ["COMMAND","RSS", ProcName, MemSize_rss] = 
					 string:tokens(Rss_Data," \n"),
				     MemSize_rss
			 end, PidList),
    %% ct:pal("MemSizes: ~p ",[MemSizes]),
    MemSizes.

get_pid(ProcName) ->
    ProcPid = rct_rpc:call(rpc, os, cmd, ["pgrep "++ProcName], 10000, noprint),
    Pid = string:tokens(ProcPid, "\n "),
    Pid.

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
	    ct:pal("End_Com_Size: ~s > ~p, Com_max_accepted_limit", 
		   [End_Com_rss, Com_max_accepted_limit]),
    	    ct:fail("Com memory usage is larger than expected!");
    	_Val ->
	    %% ct:pal("End com size: ~p", [Val]),
	    ok
    end,
    
    case list_to_integer(End_Beam_rss) of
    	Value when Value > Beam_max_accepted_limit ->
	    ct:pal("Start_Beam_rss: ~p", [Start_Beam_rss]),
	    ct:pal("End_Beam_rss: ~p", [End_Beam_rss]),
	    ct:pal("End beam size: ~s > ~p, Beam_max_accepted_limit", 
		   [End_Beam_rss, Beam_max_accepted_limit]),
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
			  ok = rct_nc_testclass1_lib:
			      nc_delete_mo_instance_no_check(SessionName, 
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
			  rct_cli:send(SessionName, 
				       "ManagedElement=1,TestRoot=1", 
				       PrintOpt), 
			  rct_cli:send(SessionName, 
				       "no TestClass1="++TestClassName, 
				       PrintOpt)
			  %% rct_cli:send(SessionName, "commit", PrintOpt)
		  end, 
		  CLI_SessionNameList),
    timer:sleep(5000),
    rct_cli:send(SessionName, "commit", PrintOpt),
    rct_cli:send(SessionName, "top", PrintOpt).

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
    {ok, TestNode} = rct_safe_imm_oi_rpc:get_testnode(rct_safe_imm_oi_rpc),
    rct_safe_imm_oi_rpc:subscribe_testnode(rct_safe_imm_oi_rpc),

    ct:pal("### reboot!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ok = rct_rs232:login(console),
    net_kernel:disconnect(TestNode),
    net_kernel:disconnect(ErlNode),
    ok = ct_telnet:send(console, "reboot"),
    timer:sleep(5000),
    ok = wait_for_testnode(node_down),
    {ok,_} = ct_telnet:expect(console, "login:", [{timeout,60000}, 
						     no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Wait for testnode up.
    %%%%
    ok = wait_for_testnode(node_up),
    net_kernel:connect(TestNode),

    %%%%
    %% Sleep to be sure that com is up and ready to be used.
    %%%%
    timer:sleep(35000).


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
	BoardType when
	BoardType == "dus4101";
	BoardType == "duw4101" ->
	    CPULIMIT_FROM_PATH = ?CPULIMIT_FROM_PATH,
	    NrOfCores = 4;
	BoardType when
	BoardType == "tcu03";
	BoardType == "tcu0401";
	BoardType == "c608";
	BoardType == "dus3201";
	BoardType == "dus3201tk";
	BoardType == "dus3301";
	BoardType == "dus6303";
	BoardType == "dus6502" ->
	    %%% AXM5512 12-core CPU, 8 cores powered-up tcu03, tcu0401, c608
	    %%% AXM5508 8-core CPU dus3201
	    %%% AXM5608 8-core CPU dus3301, dus6303(MacroRBS), dus6502(MicroRBS), dus3201tk
	    CPULIMIT_FROM_PATH = ?CPULIMIT_FROM_PATH_ARM,
	    NrOfCores = 8;
	_ -> % dus5201, dus5301, dus5201tk
	    %%% AXM5512 12-core CPU dus5201
	    %%% AXM5612 12-core CPU dus5301 dus5201tk
	    CPULIMIT_FROM_PATH = ?CPULIMIT_FROM_PATH_ARM,
	    NrOfCores = 12
    end,
    {ok,_} =  rct_scp:to_target(node1, CPULIMIT_FROM_PATH, 
				?CPULIMIT_TO_PATH, 10),

    %%%%
    %% cpulimit program will controll wanted load on CPUs.
    %%%%
    lists:foreach(
      fun(_C)->
	      [] = 
		  rct_rpc:call(rpc, os, cmd, 
			       [?CPULIMIT_TO_PATH++
				    "cpulimit -z -i -l "++
				    LoadProcent++
				    " cat /dev/zero > /dev/null &"], 
			       10000, noprint)
      end, lists:seq(1,NrOfCores)),

    CatCmdsPids = rct_rpc:call(rpc, os, cmd, ["pgrep cat"], 10000, noprint),
    %% Build a List with Pids, remove \n from the received list.
    CatPidList = string:tokens(CatCmdsPids, "\n"),
    ct:pal("CatPidList:~p", [CatPidList]),
    case BoardType of
	BoardType when
	BoardType == "dus4101";
	BoardType == "duw4101" ->
	    4 = length(CatPidList);
	BoardType when
	BoardType == "tcu03";
	BoardType == "tcu0401";
	BoardType == "c608";
	BoardType == "dus3201";
	BoardType == "dus3201tk";
	BoardType == "dus3301";
	BoardType == "dus6303";
	BoardType == "dus6502" ->
	    8 = length(CatPidList);
	_ -> % dus5201, dus5301, dus3201tk
	    12 = length(CatPidList)
    end,

    %% %% CpuLimitPids = rct_rpc:call(rpc, os, cmd, ["pgrep cpulimit"], 
    %% %% 10000, noprint),
    %% %% Build a List with Pids, remove \n from the received list.
    %% CpuLimitPidList = string:tokens(CpuLimitPids, "\n"),
    %% ct:pal("CpuLimitPidList:~p", [CpuLimitPidList]),
    %% %% Check nr of expecting pids is started.
    %% 4 = length(CpuLimitPidList),

    %% Check CPU load.
    {ok, CpuLoad} = rct_tlib:cpuload(identifier,100),
    lists:foreach(fun({_, Load})->
    			  case Load > 20 of
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
%% @doc
%% Get CXS label. <br/>
%% Get SW version using COM cli interface. <br/>
%% @spec get_sw_version() -> 'CXS_LABEL'
%% @end
%% ===========================================================================
get_sw_version() ->
    CXS_label = rct_tlib:get_sw_version(cli1),
    CXS_label.

%%-------------------------------------------------------------------- 
%% @hidden
%%-------------------------------------------------------------------- 
wait_for_testnode(NodeState)->
    wait_for_testnode(NodeState, rct_safe_imm_oi_rpc).
wait_for_testnode(NodeState, InstanceName) ->	   
    receive
	{InstanceName, {NodeName, NodeState}} ->
	    ct:pal("NodeName: ~p , NodeState: ~p .",[NodeName, NodeState]),
	    ok
    after 120000 ->
	    ct:fail({"Expected NodeState not rceived",NodeState})
    end.


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
check_oi_exist() ->
    %%%%
    %% Check OI exist, check for ImplementerOne,
    %%%%
    case rct_rpc:call(rpc, safs_imm_db, ci_get, ['TESTMOMTestClass1'], 
		      10000, noprint) of
	{_,<<"ImplementerOne">>} ->
		ok;
	{_,<<"ImplementerOne">>, _} ->
	    ok;
	    Ans ->
	    ct:pal("~p", [Ans]),
	    ct:fail(" Expecting OI does not exist! ")
    end,
    ok.

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



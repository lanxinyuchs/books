%%% #0.    BASIC INFORMATION
%% coding: latin-1
%%% ----------------------------------------------------------
%%% %CCaseFile:	xl_meas_mem_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R3A/R4A/2
%%% 
%%% @doc == Measure memory usages when users operates via netconf or cli. Paralell operations to increase nr of operations. ==
%%% This Test Suite can be used on target enviroment.
%%% Used sw version and measured data is written to a file.
%%% Path: /proj/rcs/measurements/
%%%
%%% <br/><br/>
%%% 
%%% @end

-module(xl_meas_mem_SUITE).
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
%%% R2A/3      2013-11-29 etxivri     Added HW type on Filename,
%%%                                   to separate measured files.
%%% R2A/4      2013-11-29 etxivri     Use of rct_tlib to add HW type on FileName
%%% R2A/5      2013-12-02 etxivri     Update str when use writedatatofile.
%%% R2A/6      2014-01-16 etxkols     Support for dus5201. 
%%% R2A/7      2014-03-07 etxivri     Update to add Branch file name when 
%%%                                   other than Branch R2A is used.
%%% R2A/8      2014-06-04 etxivri     Changed check for "du1 login" prompt to 
%%%                                   "login:" 
%%% R2A/10     2014-07-08 etxivri     Update due to limit of nr off ssh session
%%%                                    is set to 5 at same time
%%% R3A/1      2014-12-01 etxivri     Use SwInventory when get sw version.
%%% R4A/1      2015-10-06 etxmlar     Allow 2 CPU:s to have lower background load
%%%                                   than expected.
%%% R4A/2      2016-02-17 etxkols     Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
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
	 nc_paralell_operation_10_hour_no_check_bgl/1,
	 cli_paralell_operation_10_hour_no_check_bgl/1,
	 nc_cli_paralell_operation_10_hour_no_check_bgl/1
	]).


%% -define(CLI_Name, cli1). %% CLI hook name

-define(NrOfNetConfUsers, 4). %% one is check_session.
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

-define(LOG_DIR, "/proj/rcs/measurements/xl/").
%% -define(LOG_DIR, "/home/etxivri/tmp/").

-define(CPULIMIT_FROM_PATH, "/proj/rcs/misc/cpulimit").
-define(CPULIMIT_FROM_PATH_ARM, " /proj/rcs/misc/cpulimit_arm/cpulimit").
-define(CPULIMIT_TO_PATH, "/home/sirpa/dev_patches/").

-define(IMPL_NAME_1, "ImplementerOne").
-define(IMM_TEST_CLASS1, "TESTMOMTestClass1").
-define(INSTANCE_NAME, instance_1).
-define(ATTR, "12345").

-define(NC_SubList, lists:sublist(?NC_SessionNameList,10)).
-define(CLI_SubList, lists:sublist(?CLI_SessionNameList,10)).
-define(PRINT_OPT, noprint).

-define(CHECK_SESSION, check_session).

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

    [{timetrap, {hours, 200}}, % 10 hours
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 %% {cth_conn_log, []},
		 {rct_consserv,cs1},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
					       []}}]}},
                 {rct_rs232,console},
		 {rct_netconf, check_session},
		 {rct_cli, {cli1, [manual_connect]}},
		 {rct_safe_rpc,[{safe_services, 
				 [{imm, 0}]}, %% debuglevel 0
				{instance_name, rct_safe_imm_oi_rpc}, 
				{finalize_on_fail, true}]},
		 {rct_tlib,{identifier,[]}},
		 {rct_scp, [{1, node1}]},
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
	    C = rct_rpc:call(rpc, os, cmd, ["pgrep netconf"], 10000, noprint),
	    D = rct_rpc:call(rpc, os, getpid, [], 10000),

	    ct:pal("### Com: ~p",[A]),
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
    rct_rpc:call(rpc, os, cmd, ["pkill cat"], 10000, noprint),
    rct_rpc:call(rpc, os, cmd, ["rm "++?CPULIMIT_TO_PATH++"cpulimit"], 10000, 
		 noprint),

    [] = rct_rpc:call(rpc, os, cmd, ["pgrep cat"], 10000, noprint),
    [] = rct_rpc:call(rpc, os, cmd, ["pgrep cpulimit"], 10000, noprint),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    ok.


nc_open({error,_}, Name) ->
    %% Clean up at our client, if netconf process was killed on node, 
    ct_netconfc:close_session(Name, 30000), 
    ct_netconfc:open(Name, [{timeout, 60000}]), % Then set up again.
    nc_delete_mo_no_check(Name, ?NC_SessionNameList),
    throw({?MODULE, found});
nc_open({ok,_}, Name) ->
    nc_delete_mo_no_check(Name, ?NC_SessionNameList),
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
%% @spec nc_paralell_operation_10_hour_no_check_bgl(Config) -> ok
%% @end
%%--------------------------------------------------------------------
%%%%%%%%%%%%%%%% NC %%%%%%%%%%%%%%%%%%%%
nc_paralell_operation_10_hour_no_check_bgl(Config) ->
    reboot_node(),
    generate_cpu_load("50"),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_paralell_operation(10, LogPath, nc).

%%%%%%%%%%%%%%%% CLI %%%%%%%%%%%%%%%%%%%%
cli_paralell_operation_10_hour_no_check_bgl(Config) ->
    reboot_node(),
    generate_cpu_load("50"),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_paralell_operation(10, LogPath, cli).

%%%%%%%%%%%%%%%% NC and CLI %%%%%%%%%%%%%%%%%%%%
nc_cli_paralell_operation_10_hour_no_check_bgl(Config) ->
    reboot_node(),
    generate_cpu_load("50"),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_paralell_operation(10, LogPath, {nc, cli}).

%% ===========================================================================
%% @doc
%% This will do netconf operations and measure used memory size. <br/>
%% Fragment shall be owened by an application <br/>
%% The measured data will be stored in file. <br/>
%% Hours = integer() <br/>
%% LogPath = Path to ct logs directory. <br/>
%% OperTyp = nc | cli | {nc, cli}
%% @spec do_meas_mem_paralell_operation(Hours, LogPath, OperTyp) -> ok
%% @end
%% ===========================================================================
do_meas_mem_paralell_operation(Hours, LogPath, OperType) ->
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
    TestNodeBeamPid = rpc:call(TestNode, os, getpid, [], 5000),
    ct:pal("TestNodeBeamPid: ~p .",[TestNodeBeamPid]),

    ComPid = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 
			  10000, noprint),
    ct:pal("### Com: ~p",[ComPid]),

    StartNrOfInst = get_nr_of_testclass1_inst(?CHECK_SESSION),
    ct:pal("### StartNrOfInst: ~p",[StartNrOfInst]),


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

	case OperType of
	    nc ->
		perform_paralell_operations_once_no_check(
		  ?NC_SessionNameList, OperType);
	    cli ->
		perform_paralell_operations_once_no_check(
		  ?CLI_SessionNameList, OperType);
	    {nc, cli} ->
		perform_paralell_operations_once_no_check(
		  ?NC_SubList, nc),
		perform_paralell_operations_once_no_check(
		  ?CLI_SubList, cli)
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

	Nr = loop_paralell_operations(StartTime, 
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
				      OperType
				     ),
        %%%%
	%% get used memory size
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
	FileNameOperType = case OperType of
			       nc -> "nc";
			       cli -> "cli";
			       {nc, cli} -> "nc_cli"
			   end,
	FileNameStr = FileNameOperType++
	    "_operation_"++
	    integer_to_list(Hours)++
	    "_hour_no_check",
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

	EndTime = os:timestamp(),
	End_Time = 
	    trunc(timer:now_diff(EndTime, StartTime) / 1000 / 1000),
	%% Needed for Jockes plot.
	ct:log("~p; Nr: ~p; Time: ~p; "
	       "Start_tot_bc: ~s; End_tot_bc: ~s; diff_tot_bc: ~p ; "
	       "Start_com: ~s; End_com: ~s; diff_com: ~p ; "
	       "Start_beam: ~s; End_beam: ~s; diff_beam: ~p;  ~n", 
	       [httpd_util:rfc1123_date(),
		Nr,
		End_Time,
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
	    ct:pal("Reason:~p",[Reason]),
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
    %% Cleanup Sessions
    %%%%
    EndNrOfInst = get_nr_of_testclass1_inst(?CHECK_SESSION),
    ct:pal("### EndNrOfInst: ~p",[EndNrOfInst]),

    %%%%
    %% Cleanup sessions.
    %%%%
    case OperType of
	nc ->
	    lists:foreach(
	      fun(Name3) ->
		      ct_netconfc:open(Name3,[{timeout, 30000}]),
		      TestClassName = atom_to_list(Name3),
		      rct_nc_testclass1_lib:
			  nc_delete_mo_instance_no_check(Name3, 
							 TestClassName),
		      ct_netconfc:close_session(Name3, 30000)
	      end, ?NC_SessionNameList);
	    
	cli ->
	    lists:foreach(
	      fun(Name4) ->
		      TestClassName1 = atom_to_list(Name4),
		      %% rct_cli:disconnect(Name4, noprint)
		      rct_cli:connect(Name4, ?PRINT_OPT),
		      rct_cli_testclass1_lib:
			  delete_mo_inst(Name4, TestClassName1, ?PRINT_OPT),
		      rct_cli:disconnect(Name4, ?PRINT_OPT)
	      end, ?CLI_SessionNameList);
	{nc, cli} ->
	    lists:foreach(
	      fun(Name5) ->
		      ct_netconfc:open(Name5,[{timeout, 30000}]),
		      TestClassName = atom_to_list(Name5),
		      rct_nc_testclass1_lib:
			  nc_delete_mo_instance_no_check(Name5, 
							 TestClassName),
		      ct_netconfc:close_session(Name5, 30000)
	      end, ?NC_SubList++?CLI_SubList)
	    %% ct:fail("Not impl yet!",[])
    end,



    [] = rct_rpc:call(rpc, os, cmd, ["pgrep netconf"], 10000, noprint),

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
loop_paralell_operations(StartTime, Duration, Start_Tot_bc, Start_Com_rss, 
			 Start_Beam_rss,Start_TestBeam_rss, Start_test_oi,
			 Start_ift_app, Start_test_app, LogPath, BeamPid, 
			 TestNodeBeamPid, OperType) ->
    loop_paralell_operations(StartTime, Duration, 0, Start_Tot_bc, 
			     Start_Com_rss, Start_Beam_rss, 
			     Start_TestBeam_rss, Start_test_oi,
			     Start_ift_app, Start_test_app, LogPath, BeamPid, 
			     TestNodeBeamPid, OperType).

loop_paralell_operations(StartTime, Duration, Nr, Start_Tot_bc, Start_Com_rss,
			 Start_Beam_rss, Start_TestBeam_rss, Start_test_oi,
			 Start_ift_app, Start_test_app, LogPath, BeamPid, 
			 TestNodeBeamPid, OperType) ->

    NrOfInst = get_nr_of_testclass1_inst(?CHECK_SESSION),
    ct:pal("¤¤¤ Perform paralell operation, nr: ~p, "
	   "NrOfExistingInst: ~p", [Nr, NrOfInst]),

    %% StartTimeStamp = os:timestamp(),

    case OperType of
	nc ->
	    perform_paralell_operations_once_no_check(
	      ?NC_SessionNameList, OperType);
	cli ->
	    perform_paralell_operations_once_no_check(
	      ?CLI_SessionNameList, OperType);
	{nc, cli} ->
	    perform_paralell_operations_once_no_check(
	      ?NC_SubList, nc),
	    perform_paralell_operations_once_no_check(
	      ?CLI_SubList, cli)
    end,

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
	    DiskNr = string:str(Df, ["/disk"]),
	    UsedDisk = lists:nth(DiskNr-1, Df),

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
	   loop_paralell_operations(StartTime, Duration, Nr+1, Start_Tot_bc, 
			   Start_Com_rss, Start_Beam_rss, Start_TestBeam_rss,
			   Start_test_oi, Start_ift_app, Start_test_app,
			   LogPath, BeamPid, TestNodeBeamPid, OperType)
    end.

%% ===========================================================================
%% @doc
%% Create MO, Delete MO, no check. if something goes wrong then continue <br/>
%% @spec perform_paralell_operations_once_no_check(SessionNameList, OperType) -> ok
%% @end
%% ===========================================================================
perform_paralell_operations_once_no_check(SessionNameList, OperType) ->
    Self = self(),  
 
    %%%%
    %% Add instances
    %%%%
    PidList_add = 
	lists:map(
	  fun(Name1) ->
		  spawn(fun() ->
				case OperType of
				    nc ->
					nc_add_cmd_no_check(Name1),
					Self ! add_done;
				    cli ->
					cli_add_cmd_no_check(Name1),
					Self ! add_done;
				    {nc, cli} ->
					ct:fail("Not impl yet!",[])
				end
			end)
	  end,
	  SessionNameList),

    %%%%
    %%Wait on answer from request.
    %%%%
    lists:foreach(fun(_PidA) ->
    			 receive
    			      add_done -> 			      
    				 ok
    			 after 30000 -> 
    				 ct:log("Error no answer within time, Add!", [])
    			 end
    		  end, PidList_add),

    %%%%
    %% sync.
    %%%%
    os_cmd_sync(),

    %%%%
    %% Delete instances
    %%%%
    %% Self = self(),    
    PidList_delete = 
	lists:map(
	  fun(Name2) ->
		  spawn(fun() -> 
				case OperType of
				    nc ->
					nc_delete_cmd_no_check(Name2),
					Self ! delete_done;
				    cli ->
					cli_delete_cmd_no_check(Name2),
					Self ! delete_done;
				    {nc, cli} ->
					ct:fail("Not impl yet!",[])
				end
			end)
	  end,
	  SessionNameList),

    %%%%
    %%Wait on answer from request.
    %%%%
    lists:foreach(fun(_PidA) ->
    			 receive
    			      delete_done -> 			      
    				 ok
    			 after 30000 -> 
    				 ct:log("Error no answer within time, Del!", [])
    			 end
    		  end, PidList_delete),

    %%%%
    %% sync.
    %%%%
    os_cmd_sync(),

    %%%%
    %% Cleanup sessions.
    %%%%
    cleanup_sessions(OperType),
    
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% netconf. <br/>
%% @end
%%--------------------------------------------------------------------
nc_add_cmd_no_check(SessionName) ->
    case ct_netconfc:open(SessionName, [{timeout, 30000}]) of
    	{ok,_} ->
    	    ok;
    	Ret ->
    	    ct:log("~p, Fail to open NC session: ~p ",[SessionName, Ret])
    end,
    
    TestClassName = atom_to_list(SessionName),

    case rct_nc_testclass1_lib:
	nc_add_mo_instance_and_attr_no_check(
	  SessionName, TestClassName, ?ATTR) of
	ok ->
	    case ct_netconfc:close_session(SessionName, 30000) of
	    	ok ->
	    	    ok;
	    	Res ->
	    	    ct:log("~p, Fail to close session: ~p ",[SessionName, Res])
	    end;
	Res1  ->
	    ct:log("~p, Fail to add TestClass1: ~p ",[SessionName, Res1])
    end.

%%%%%%%%%%%%%%
nc_delete_cmd_no_check(SessionName) ->
    case ct_netconfc:open(SessionName, [{timeout, 30000}]) of
    	{ok,_} ->
    	    ok;
    	Ret ->
    	    ct:log("~p, Fail to open NC session: ~p ",[SessionName, Ret])
    end,
    
    TestClassName = atom_to_list(SessionName),
    
    case rct_nc_testclass1_lib:
	nc_delete_mo_instance_no_check(SessionName, 
				       TestClassName) of
	ok ->
	    case ct_netconfc:close_session(SessionName, 30000) of
	    	ok ->
	    	    ok;
	    	Res ->
	    	    ct:log("~p, Fail to close session: ~p ",[SessionName, Res])
	    end;
	    %% ok;
	Res1 ->
	    ct:log("~p, Fail to delete TestClass1: ~p ",[SessionName, Res1])
    end.

%%--------------------------------------------------------------------
%% @doc 
%% cli. <br/>
%% @end
%%--------------------------------------------------------------------
cli_add_cmd_no_check(SessionName) ->
    PrintOpt = ?PRINT_OPT,
    case rct_cli:connect(SessionName, PrintOpt) of
	ok ->
	    ok;
	Ret ->
	    ct:log("Fail to open CLI session:~p,  ~p ",
		   [SessionName, Ret])
    end,
    
    TestClassName = atom_to_list(SessionName),

    case rct_cli_testclass1_lib:
	add_mo_inst_and_attr(SessionName, TestClassName,
			     ?ATTR, PrintOpt) of
	ok ->
	    ok;
	Res1  ->
	    ct:log("~p, Fail to add TestClass1: ~p ",[SessionName, Res1])
    end,
    
    case rct_cli:disconnect(SessionName, PrintOpt) of
	ok ->
	    ok;
	Res2 ->
	    ct:log("Fail to close cli session: ~p,  ~p ",
		   [SessionName, Res2])
    end.

%%%%%%%%%%%%%%
cli_delete_cmd_no_check(SessionName) ->
    PrintOpt = ?PRINT_OPT,
    case rct_cli:connect(SessionName, PrintOpt) of
	ok ->
	    ok;
	Ret1 ->
	    ct:log("Fail to open CLI session: ~p,  ~p ",
		   [SessionName, Ret1])
    end,
    
    TestClassName = atom_to_list(SessionName),

    case rct_cli_testclass1_lib:
	delete_mo_inst(SessionName, TestClassName, PrintOpt) of
	ok ->
	    ok;
	Res  ->
	    ct:log("~p, Fail to delete TestClass1: ~p ",[SessionName, Res])
    end,
    
    case rct_cli:disconnect(SessionName, PrintOpt) of
	ok ->
	    ok;
	Res2 ->
	    ct:log("Fail to close cli session: ~p,  ~p ",
		   [SessionName, Res2])
    end.

%%--------------------------------------------------------------------
%% @doc 
%% os_cmd_sync()  <br/>
%% @end
%%--------------------------------------------------------------------
os_cmd_sync() ->
    case rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint) of
    	[] ->
    	    ok;
    	Result ->
    	    ct:log("Fail to send sync via rpc: ~p.\n Check top ",[Result]),
    	    rct_rs232:login(console),
    	    ct_telnet:cmd(console, "top -b -n 1", 30000)
    end.
%%--------------------------------------------------------------------
%% @doc 
%% cleanup_sessions()  <br/>
%% @end
%%--------------------------------------------------------------------
cleanup_sessions(OperType) ->
    case OperType of
	nc ->
	    lists:foreach(fun(Name3) ->
				  ct_netconfc:close_session(Name3, 2000)
			  end, ?NC_SessionNameList);	    
	cli ->
	    lists:foreach(fun(Name4) ->
				  rct_cli:disconnect(Name4, noprint)
			  end, ?CLI_SessionNameList);
	{nc, cli} ->
	    ct:fail("Not impl yet!",[])
    end,

    case rct_rpc:call(rpc, os, cmd, ["pgrep netconf"], 10000, noprint) of
	[] ->
	    ok;
	_ ->
	    rct_rpc:call(rpc, os, cmd, ["pkill netconf"], 10000, noprint)
    end,

    case rct_rpc:call(rpc, os, cmd, ["pgrep cli"], 10000, noprint) of
    	[] ->
    	    ok;
    	_ ->
    	    rct_rpc:call(rpc, os, cmd, ["pkill cli"], 10000, noprint)
    end,

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
			  Free = rct_rpc:call(rpc, os, cmd, ["free"], 10000, 
					      noprint),
			  {match,[Tot_bc]} = 
			      re:run(Free,"buffers/cache: *([0-9]+).*",
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

%% Used by cleanup if tc fail.
%% ===========================================================================
%% @doc
%% Delete MO no check. Used in cleanup if TC fail<br/>
%% - Delete MO. <br/>
%% @spec nc_delete_mo_no_check(SessionName, NrList) -> ok
%% @end
%% ===========================================================================
nc_delete_mo_no_check(SessionName, NC_SessionNameList)->
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
	BoardType when BoardType == "dus4101";
		       BoardType == "duw4101" ->
	    4 = length(CatPidList);
	"tcu03" ->
	    8 = length(CatPidList);
	_ -> % dus5201, dus3201
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
get_nr_of_testclass1_inst(NC_session) ->
    {ok,_} = ct_netconfc:open(NC_session, [{timeout, 60000}]),
    TestRootInstances = 
	rct_nc_testclass1_lib:
	nc_get_all_under_testroot(NC_session),
    NrOfTestRootInst = length(TestRootInstances),
ok = ct_netconfc:close_session(NC_session, 60000),
NrOfTestRootInst.

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

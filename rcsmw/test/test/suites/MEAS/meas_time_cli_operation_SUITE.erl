%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	meas_time_cli_operation_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R3A/R4A/R5A/2
%%% 
%%% @doc == Measure times when one cli user add, show, delete several mo instances.==
%%%
%%% 
%%% @end

-module(meas_time_cli_operation_SUITE).
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
%%% R2A/2      2013-10-10 etxivri     Created
%%% R2A/3      2013-10-11 etxivri     Changed to show all and some other updates.
%%% R2A/4      2013-10-22 etxivri     Added ##LogDirX as key used for plot.
%%% R2A/5      2013-11-08 etxivri     Update check that implementer is created.
%%% R2A/6      2013-11-21 etxivri     Some cleanup.
%%% R2A/7      2013-12-02 etxivri     HW type added to measured File.
%%%                                   Change in the check to se when expected
%%%                                   nr of inst exist or not.
%%%                                   Remove repeat of show operation.
%%% R2A/8      2013-12-04 etxivri     Change name on cli sessions used for check
%%% R3A/1      2014-12-11 etxivri     Update get sw vversion.
%%% R4A/1      2015-09-30 etxivri     Changed now() to os:timestamp().
%%% R5A/1      2015-10-07 etxivri     Update for R5
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
	 add_del_several_mo_inst_attr_nroftimes/1,
	 add_del_several_mo_inst_attr_one_commit_and_show_oper/1
	]).

-define(NrOfCliUsers, 1).

-define(NrOfMoInstList, lists:seq(1, 50) ).
%% -define(NrOfTimes, 10).
-define(NrOfTimes, 5).
%% -define(NrOfMoInstList, lists:seq(1,2) ).
%% -define(ShowNrOfTimes, 10).

-define(CLI_SessionNameList, 
	[list_to_atom(
	   "cli"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfCliUsers)]).

-define(IMPL_NAME_1, "ImplementerOne").
-define(IMM_TEST_CLASS1, "TESTMOMTestClass1").

%% -define(LOG_DIR, "/proj/rcs/measurements/").
-define(LOG_DIR, "/proj/rcs/measurements/nc_cli_operations/").
%% -define(LOG_DIR, "/home/etxivri/tmp/").

-define(CHECK_SESSION, cli_check_session).
%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% runs the rct_netconf hook for each user, to be able to open a  <br/>
%% netconf session for each user. <br/>
%% @end
%%--------------------------------------------------------------------
suite() -> 
    %% build up a list of cli touples with differents Names.
    CliHooks = 
	[{rct_cli, {list_to_atom("cli"++integer_to_list(N)), 
		    [manual_connect]}} || N <- lists:seq(1,?NrOfCliUsers)],

    [{timetrap, {hours, 2}}, % 2 hours
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 %% {cth_conn_log, []},
		 {rct_power,node},
		 {rct_consserv,cs1},
                 {rct_rs232,console},
		 {rct_tlib,{identifier,[]}},
		 {rct_core,[]},
		 {rct_cli, {cli_check_session, [manual_connect]}},
		 {rct_safe_rpc,[{safe_services, 
				 [{imm, 0}]}, %% debuglevel 0
				{instance_name, rct_safe_imm_oi_rpc}, 
				{finalize_on_fail, true}]},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT",
						"CRASH REPORT"],
					       []}}]}} | 
		 %% {rct_logging, {all, [{erlang,{[],[]}}]}} |
		 CliHooks
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
    	    ct:pal("Testcase failed due to: ~p.  \nClean up", [Reason]),

	    A = rct_rpc:call(rpc, os, cmd, 
			     ["pgrep -f com/bin/com -u $USER"], 10000, noprint),
	    C = rct_rpc:call(rpc, os, cmd, ["pgrep cli"], 10000, noprint),
	    D = rct_rpc:call(rpc, os, getpid, [], 10000),
    	    ct:pal("### Com: ~p",[A]),
	    ct:pal("### cli: ~p",[C]),
	    ct:pal("### BeamPid: ~p .",[D]),

	    OI = rct_rpc:call(rpc, safs_imm_db, ci_get, 
			      ['TESTMOMTestClass1'], 10000, noprint),
	    ct:pal("~p.", [OI])
    end,

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases defined in list.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [
     add_del_several_mo_inst_attr_nroftimes,
     add_del_several_mo_inst_attr_one_commit_and_show_oper
    ].

%%--------------------------------------------------------------------
%% @doc
%% TCs runed by jenkins.
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @doc
%% Times for one Cli users, add, delete MO instance and attributes. <br/>
%% Repeat several times. <br/>
%% - Open cli session. <br/>
%%   Start measure,  <br/>
%%   add 500 MO inst and attributes.<br/>
%%   Commit, <br/>
%%   Stop measure.  <br/>
%% - Close cli session. <br/>
%% - Check MO inst is created. <br/>
%% - Open session. <br/>
%%   Start measure, <br/>
%%   Delete 500 MO and attributes inst. <br/>
%%   Commit.<br/>
%%   Stop measure. <br/>
%% - Close cli session.<br/>
%% - Check MO inst is deleted.<br/>
%%
%% - print the operation measure times<br/>
%% @spec add_del_several_mo_inst_attr_nroftimes(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
add_del_several_mo_inst_attr_nroftimes(_Config) ->
    ct:pal("### Times for one Cli user, add, delete  ~p MO instances and "
	   "attributes. One Commit!",[length(?NrOfMoInstList)]),

    %%%%
    %% Get Beam Pid. Will be used when get memsize.
    %%%%
    BeamPid = rct_rpc:call(rpc, os, getpid, [], 10000),
    ct:pal("BeamPid: ~p .",[BeamPid]),

    %%%%
    %% Get testnode Beam Pids.
    %%%%
    {ok, TestNode} = rct_safe_imm_oi_rpc:get_testnode(rct_safe_imm_oi_rpc),
    TestNodeBeamPid = rpc:call(TestNode, os, getpid, [], 10000),
    ct:pal("TestNodeBeamPid: ~p .",[TestNodeBeamPid]),

    ComPid = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 
			  10000, noprint),
    ct:pal("### Com: ~p",[ComPid]),

    %%%%
    %% Connect a OI for the testfragment
    %%%%
    Handle = create_oi(?IMPL_NAME_1, ?IMM_TEST_CLASS1),

    [Cli_session|_T]=?CLI_SessionNameList,
    PrintOpt = noprint,
    NrOfLoop = lists:seq(1, ?NrOfTimes),

    try
	check_oi_exist(),

        %%%%
	%% Add Mo inst of testClass1
        %%%%
	ct:pal("Add ~p MO instances and attr, one commit. Repeat: ~p times.",
	       [length(?NrOfMoInstList), NrOfLoop]),
	ok = rct_cli:connect(Cli_session, PrintOpt),
	AllTimeToAdd = 
	    lists:map(
	      fun(LoopNr) ->
		      InstNamesList = 
			  lists:map(
			    fun(Nr)->
				    InstName = atom_to_list(Cli_session)++
					"_"++integer_to_list(LoopNr)++
					"_"++integer_to_list(Nr),
				    InstName
			    end, ?NrOfMoInstList),
		      AttrName = integer_to_list(LoopNr),
		      Start = os:timestamp(),
		      rct_cli_testclass1_lib:
			  add_several_mo_inst_and_attr(Cli_session, 
						       InstNamesList, 
						       AttrName,
						       PrintOpt),
		      End = os:timestamp(),
		      TimeToAdd = 
		      	  trunc(timer:now_diff(End, Start) / 1000 / 1000),
		      %%%%
		      %% Check MO instances created.
                      %%%%
		      %% ct:pal("check MO instances is created.", []),
		      CheckAddStart= os:timestamp(),
		      NrOfExpMo = LoopNr*length(InstNamesList),
		      wait_for_nr_of_exp_mo_inst_exist(?CHECK_SESSION,
						       NrOfExpMo,
						       PrintOpt),
		      CheckAddEnd  = os:timestamp(),
		      TimeToMoInstExist = 
			  trunc(timer:now_diff(CheckAddEnd, 
					       CheckAddStart) / 1000 / 1000),
		      ct:pal("LoopNr: ~p, TimeToAdd and one commit ~p MO "
			     "inst and attr: ~p sec, "
			     "TimeToMoInstExist: ~p .",[LoopNr,
							length(InstNamesList),
							TimeToAdd, 
							TimeToMoInstExist]),
		      {TimeToAdd, TimeToMoInstExist}
	      end, NrOfLoop),
	ok = rct_cli:disconnect(Cli_session, PrintOpt),
	TotAllTimeToAdd = 
	    lists:foldl(fun({TimeToAdd, _TimeToMoInstExist}, Sum) -> 
				TimeToAdd + Sum
			end, 0, AllTimeToAdd),
	
	[] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),
	
        %%%%
	%% Delete MO instances.
        %%%%
	ct:pal("Delete ~p MO instances, one commit. Repeat: ~p times.",
	       [length(?NrOfMoInstList), NrOfLoop]),
	ok = rct_cli:connect(Cli_session, PrintOpt),
	AllTimeToDel = 
	    lists:map(
	      fun(LoopNr) ->
		      InstNameList = 
			  lists:map(
			    fun(Nr) ->
				    InstName = atom_to_list(Cli_session)++
					"_"++integer_to_list(LoopNr)++
					"_"++integer_to_list(Nr),
				    InstName
			    end, ?NrOfMoInstList),
		      Start = os:timestamp(),
		      ok = rct_cli_testclass1_lib:
			  delete_several_mo_inst(Cli_session, 
						 InstNameList, 
						 PrintOpt),
		      End = os:timestamp(),
		      TimeToDel = 
			  trunc(timer:now_diff(End, Start) / 1000 / 1000),
		      %%%%
		      %% Check MO instance deleted.
                      %%%%
		      %% ct:pal("check MO instances is deleted.", []),
		      CheckDelStart = os:timestamp(),
		      NrOfExpMo = (?NrOfTimes-LoopNr)*length(InstNameList),
		      wait_for_nr_of_exp_mo_inst_exist(?CHECK_SESSION,
						       NrOfExpMo,
						       PrintOpt),
		      CheckDelEnd = os:timestamp(),
		      TimeToMoInstDel = 
			  trunc(timer:now_diff(CheckDelEnd, 
					       CheckDelStart) / 1000 / 1000),
		      ct:pal("LoopNr: ~p, TimeToDel and one commit ~p "
			     "MO inst: ~p sec, "
			     "TimeToMoInstDel: ~p .",[LoopNr,
						      length(InstNameList),
						      TimeToDel,
						      TimeToMoInstDel]),
		      {TimeToDel, TimeToMoInstDel}
	      end, NrOfLoop),
	ok = rct_cli:disconnect(Cli_session, PrintOpt),

	TotAllTimeToDel = 
	    lists:foldl(fun({TimeToDel, _TimeToMoInstDel}, Sum) -> 
				TimeToDel + Sum
			end, 0, AllTimeToDel),
	
	[] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),
	
	All_TimeToAdd = lists:map(fun({TimeToAdd, _TimeToMoInstExist}) ->
					  TimeToAdd
				  end, AllTimeToAdd),
	All_TimeToDel = lists:map(fun({TimeToDel, _TimeToMoInstDel}) ->
					  TimeToDel
				  end, AllTimeToDel),
	%%%%
	%% Write info to file
        %%%%
	CXS_label = get_sw_version(),
	[{_, NodeName}] = ct:get_config(test_nodes),
	%% FileName = rct_tlib:
	%%     get_filename("cli_add_delete_several_mo_inst_attr_nroftimes"),
	BoardType = meas_lib:get_board_type(),
	Branch = meas_lib:get_branch(),
	FileName = Branch++"_"++BoardType++"_cli_add_delete_several_mo_inst_attr_nroftimes",
	rct_tlib:writeDataToFile(?LOG_DIR, 
				 FileName, 
				 "~p;~w;"
				 "~p;~p;~p;~p;~p;~p;~p;~p;~p;~p;~p;~p;~w~n", 
				 [httpd_util:rfc1123_date(),
				  CXS_label,
				  length(?NrOfMoInstList),
				  ?NrOfTimes]++
				     All_TimeToAdd++
				     All_TimeToDel++ 
				     [NodeName]
				),

	%%%%
	%% Print out some results.
        %%%%
	ZipTimes = lists:zip(AllTimeToAdd, AllTimeToDel),
	lists:foreach(fun({{TimeToAdd, TimeToMoInstExist}, 
			   {TimeToDel, TimeToMoInstDel}}) ->
			      ct:pal("### TimeToAdd: ~p sec, "
				     "TimeToMoInstExist: ~p sec,  "
				     "TimeToDel: ~p sec, "
				     "TimeToMoInstDel: ~p sec.",
				     [TimeToAdd, TimeToMoInstExist, 
				      TimeToDel, TimeToMoInstDel])
		      end, ZipTimes),
	ct:log("### TotAllTimeToAdd: ~p, AllTimeToDel: ~p, .",
	       [TotAllTimeToAdd, 
		TotAllTimeToDel]),

	ct:pal("Add, delete measured times is stored in:~n ##LogDir1: ~p~n",
	       [?LOG_DIR++FileName]),

        %%%%
	%% Delete OI for the testfragment
        %%%%
	ok = delete_oi(?IMPL_NAME_1, ?IMM_TEST_CLASS1, Handle)

    catch
    	_:Reason -> %% Case TC fail, then delete OI.
	    ct:pal("TC will fail, Reason:~p",[Reason]),
	    rct_cli:connect(Cli_session, PrintOpt),
	    lists:foreach(
	      fun(LoopNr) ->
		      InstNameList = 
			  lists:map(
			    fun(Nr) ->
				    InstName = atom_to_list(Cli_session)++
					"_"++integer_to_list(LoopNr)++
					"_"++integer_to_list(Nr),
				    InstName
			    end, ?NrOfMoInstList),
		      rct_cli_testclass1_lib:
			  delete_several_mo_inst(Cli_session, 
						 InstNameList, 
						 PrintOpt)
	      end, NrOfLoop),
	    rct_cli:disconnect(Cli_session, PrintOpt),
            %%%%
    	    %% Delete OI for the testfragment
            %%%%
   	    ok = delete_oi(?IMPL_NAME_1, ?IMM_TEST_CLASS1, Handle),
	    
	    ct:fail(Reason)
    end,
    
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Times for one Cli users, add, delete MO instance and attributes, <br/>
%% commit once. <br/>
%% - Open cli session. <br/>
%%   Start measure,  <br/>
%%   add several MO inst and attributes.<br/>
%%   Commit, <br/>
%%   Stop measure.  <br/>
%% - Close cli session. <br/>
%% - Check MO inst is created. <br/>
%% - Time to show all MO inst undet TestRoot.<br/>
%% - Time to show MO inst, one at time.<br/>
%% - Open session. <br/>
%%   Start measure,  <br/>
%%   Delete several MO and attributes inst.  <br/>
%%   Commit. <br/>
%%   Stop measure.  <br/>
%% - Close cli session. <br/>
%% - Check MO inst is deleted. <br/>
%%
%% - print the operation measure times <br/>
%% @spec add_del_several_mo_inst_attr_one_commit_and_show_oper(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
add_del_several_mo_inst_attr_one_commit_and_show_oper(_Config) ->
    ct:pal("### Times for one Cli user, add, delete  ~p "
	   "MO instances and attributes. One Commit!",
	   [?NrOfTimes*length(?NrOfMoInstList)]),

    %%%%
    %% Get Beam Pid. Will be used when get memsize.
    %%%%
    BeamPid = rct_rpc:call(rpc, os, getpid, [], 10000),
    ct:pal("BeamPid: ~p .",[BeamPid]),

    %%%%
    %% Get testnode Beam Pids.
    %%%%
    {ok, TestNode} = rct_safe_imm_oi_rpc:get_testnode(rct_safe_imm_oi_rpc),
    TestNodeBeamPid = rpc:call(TestNode, os, getpid, [], 10000),
    ct:pal("TestNodeBeamPid: ~p .",[TestNodeBeamPid]),

    ComPid = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 
			  10000, noprint),
    ct:pal("### Com: ~p",[ComPid]),

    %%%%
    %% Connect a OI for the testfragment
    %%%%
    Handle = create_oi(?IMPL_NAME_1, ?IMM_TEST_CLASS1),

    [Cli_session|_T]=?CLI_SessionNameList,
    PrintOpt = noprint,
    NrOfLoop = lists:seq(1, ?NrOfTimes),

    try
	check_oi_exist(),

	Tot_InstNameList = 
	    lists:map(
	      fun(LoopNr2) ->
		      InstNameList = 
			  lists:map(
			    fun(Nr2) ->
				    InstName2 = atom_to_list(Cli_session)++
					"_"++integer_to_list(LoopNr2)++
					"_"++integer_to_list(Nr2),
				    InstName2
			    end, ?NrOfMoInstList),
		      InstNameList
	      end, NrOfLoop),
	TotInstNameList = lists:append(Tot_InstNameList),

        %%%%
	%% Add Mo inst of testClass1
        %%%%
	AddInstAttrNamesList = 
	    lists:map(
	      fun(LoopNr) ->
		      InstAttrNamesList = 
			  lists:map(
			    fun(Nr)->
				    InstName = atom_to_list(Cli_session)++
					"_"++integer_to_list(LoopNr)++
					"_"++integer_to_list(Nr),
				    AttrName = integer_to_list(LoopNr),
				    {InstName,AttrName}
			    end, ?NrOfMoInstList),
		      InstAttrNamesList
	      end, 
	      NrOfLoop),
	TotAddInstAttrNamesList = lists:append(AddInstAttrNamesList),
	ct:log("TotAddInstAttrNamesList: ~p",[TotAddInstAttrNamesList]),
	ct:pal("Add ~p MO instances and attr.",
	       [length(TotAddInstAttrNamesList)]),
	ok = rct_cli:connect(Cli_session, PrintOpt),
	AddStart = os:timestamp(),
	ok = rct_cli_testclass1_lib:
	    add_several_mo_inst_and_attr_list_no_commit(Cli_session, 
							TotAddInstAttrNamesList,
							PrintOpt),
	AddEnd = os:timestamp(),
	rct_cli:send(Cli_session, "commit", ">", PrintOpt),
	AddCommitEnd = os:timestamp(),
	ok = rct_cli:disconnect(Cli_session, PrintOpt),
	
	TimeToAdd = 
	    trunc(timer:now_diff(AddEnd, AddStart) / 1000 / 1000),
	CommitAddEnd = 
	    trunc(timer:now_diff(AddCommitEnd, AddStart) / 1000 / 1000),
	ct:pal("TimeToaDD: ~p MO inst and attr: ~p sec, "
	       "commit after all is added. Commit end: ~p .",
	       [length(TotAddInstAttrNamesList),
		TimeToAdd, CommitAddEnd]),

        %%%%
	%% Check MO instances created.
        %%%%
	ct:pal("check MO instances is created.", []),

	ok = rct_cli:connect(Cli_session, PrintOpt),
	CheckAddStart= os:timestamp(),
	NrOfExpMo = length(TotAddInstAttrNamesList),
	wait_for_nr_of_exp_mo_inst_exist(?CHECK_SESSION,
					 NrOfExpMo,
					 PrintOpt),
	CheckAddEnd  = os:timestamp(),
	ok = rct_cli:disconnect(Cli_session, PrintOpt),
	TimeToMoInstExist = 
	    trunc(timer:now_diff(CheckAddEnd, CheckAddStart) / 1000 / 1000),
	ct:pal("TimeToMoInstExist: ~p sec, ",[TimeToMoInstExist]),
	
	[] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

	%%%%%%%%%%%%%%%%%%%%%%%
	%% cli show operation
	%%%%%%%%%%%%%%%%%%%%%%%
	NrOfExpMo = length(TotAddInstAttrNamesList),
	ct:pal("Show everything under TestRoot in one operation.", []),
	TimeToShowTestRoot = 
	    time_to_show_all_under_testroot(Cli_session, PrintOpt),

	%% Read all created mo inst, one by one.
	ct:pal("Show ~p created MO, one by one.", [length(TotInstNameList)]),
	{Tot_TimeToReadMO, 
	 Min_TimeToReadMO, 
	 Max_TimeToReadMO} = time_to_get_one_mo_inst_at_time(Cli_session, 
							     TotInstNameList, 
							     PrintOpt), 
	Avg_TimeToReadMO = round(Tot_TimeToReadMO/length(TotInstNameList)),
	Tot_TimeToReadMO_In_sec = round(Tot_TimeToReadMO/1000),

        %%%%
	%% Delete MO inst.
        %%%%
	ct:pal("Delete ~p MO instances.",[length(TotInstNameList)]),
	ok = rct_cli:connect(Cli_session, PrintOpt),
	StartDel = os:timestamp(),
        ok = rct_cli_testclass1_lib:
	    delete_several_mo_inst_no_commit(Cli_session, 
					     TotInstNameList, 
					     PrintOpt),
	
	EndDel = os:timestamp(),
        rct_cli:send(Cli_session, "commit", ">", PrintOpt),
	DelCommitEnd = os:timestamp(),
	ok = rct_cli:disconnect(Cli_session, PrintOpt),
	TimeToDel = 
	    trunc(timer:now_diff(EndDel, StartDel) / 1000 / 1000),
	CommitDelEnd = 
	    trunc(timer:now_diff(DelCommitEnd, StartDel) / 1000 / 1000),
	ct:pal("TimeToDel: ~p MO inst: ~p sec, "
	       "commit after all is deleted. Commit end: ~p .",
	       [length(TotInstNameList), TimeToDel, CommitDelEnd]),

        %%%%
	%% Check MO instance deleted.
        %%%%
	ct:pal("check MO instances is Deleted.", []),
	ok = rct_cli:connect(Cli_session, PrintOpt),
	CheckDelStart = os:timestamp(),
	wait_for_nr_of_exp_mo_inst_exist(?CHECK_SESSION,
					 0,
					 PrintOpt),
	CheckDelEnd = os:timestamp(),
	ok = rct_cli:disconnect(Cli_session, PrintOpt),
	TimeToMoInstDel = 
	    trunc(timer:now_diff(CheckDelEnd, CheckDelStart) / 1000 / 1000),
	ct:pal("TimeToMoInstDel: ~p sec, ", [TimeToMoInstDel]),

	[] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

	%%%%
	%% Write info to file
        %%%%
	CXS_label = get_sw_version(),
	[{_, NodeName}] = ct:get_config(test_nodes),
	%% FileName = rct_tlib:
	%%     get_filename("cli_add_del_several_mo_inst_attr_one_commit"),
	BoardType = meas_lib:get_board_type(),
	Branch = meas_lib:get_branch(),
	FileName = Branch++"_"++BoardType++"_cli_add_del_several_mo_inst_attr_one_commit",
	rct_tlib:writeDataToFile(?LOG_DIR, 
				 FileName, 
				 "~p;~w;~p;~p;~p;~p;~p;~p;~p;~w~n", 
				 [httpd_util:rfc1123_date(),
				  CXS_label,
				  length(TotAddInstAttrNamesList),
				  TimeToAdd,
				  CommitAddEnd,
				  TimeToMoInstExist,
				  TimeToDel,
				  CommitDelEnd,
				  TimeToMoInstDel,
				  NodeName]
				),

	%% FileName2 = rct_tlib:
	%%     get_filename("cli_show_operation_several_mo_inst"),
	FileName2 = Branch++"_"++BoardType++"_cli_show_operation_several_mo_inst",
	rct_tlib:writeDataToFile(?LOG_DIR, 
				 FileName2, 
				 %% "~p;~w;~p;~p;~p;~p;~p;~p;~p;~p;~p;~w~n",
				 "~p;~w;~p;~p;~p;~p;~p;~p;~w~n",
				 [httpd_util:rfc1123_date(),
				  CXS_label,
				  length(TotAddInstAttrNamesList),
				  TimeToShowTestRoot,
				  Tot_TimeToReadMO_In_sec,
				  Avg_TimeToReadMO,
				  Min_TimeToReadMO, 
				  Max_TimeToReadMO,
				  NodeName]
				),

	%%%%
	%% Print out some ressults.
        %%%%
	ct:pal("### TimeToAdd: ~p, CommitAddEnd: ~p, TimeToMoInstExist: ~p.\n"
	       "### TimeToDel: ~p, CommitDelEnd: ~p, TimeToMoInstDel: ~p.",
	       [TimeToAdd, 
		CommitAddEnd,
		TimeToMoInstExist,
		TimeToDel,
		CommitDelEnd,
		TimeToMoInstDel
	       ]),

	ct:pal("Show All under TestRoot: ~p sec.", [TimeToShowTestRoot]),

	ct:pal("Read ~p created MO, one by one, tot: ~p, avg: ~p, "
	       "min: ~p, max: ~p ms.",[length(TotInstNameList), 
				       Tot_TimeToReadMO_In_sec,
				       Avg_TimeToReadMO,
				       Min_TimeToReadMO, 
				       Max_TimeToReadMO]),
	
	ct:pal("Add, Del, one commit, measured times is stored in:~n "
	       "##LogDir1: ~p~n",[?LOG_DIR++FileName]),
	ct:pal("show, measured times is stored in:~n "
	       "##LogDir2: ~p~n",[?LOG_DIR++FileName2]),

        %%%%
	%% Delete OI for the testfragment
        %%%%
	ok = delete_oi(?IMPL_NAME_1, ?IMM_TEST_CLASS1, Handle)

    catch
    	_:Reason -> %% Case TC fail, then delete OI.
    	    ct:pal("TC will fail, Reason:~p",[Reason]),
	    rct_cli:connect(Cli_session, PrintOpt),
	    lists:foreach(
	      fun(LoopNr) ->
		      InstNameList = 
			  lists:map(
			    fun(Nr) ->
				    InstName = atom_to_list(Cli_session)++
					"_"++integer_to_list(LoopNr)++
					"_"++integer_to_list(Nr),
				    InstName
			    end, ?NrOfMoInstList),
		      rct_cli_testclass1_lib:
			  delete_several_mo_inst(Cli_session, 
						 InstNameList, 
						 PrintOpt)
	      end, NrOfLoop),
	    rct_cli:disconnect(Cli_session, PrintOpt),
            %%%%
    	    %% Delete OI for the testfragment
            %%%%
    	    ok = delete_oi(?IMPL_NAME_1, ?IMM_TEST_CLASS1, Handle),

	    ct:fail(Reason)
    end,
	
    ok.


%% ===========================================================================
%% Internal functions
%% ===========================================================================
%% ===========================================================================
%% @doc
%% Create Class implementer. <br/>
%% @end
%% ===========================================================================
create_oi(IMPL_NAME, IMM_TEST_CLASS) ->
    %%%%
    %% Connect a OI for the testfragment
    %%%%
    ct:pal("Initialize OI Handle", []),
    {ok, Handle, Vsn} = rct_safe_imm_oi_rpc:initialize_2( ok),
    ct:pal("Initialize ok: ~p ~p",[Handle, Vsn]),
    ct:pal("Set Implementer name: ~p", [IMPL_NAME]),
    ok = rct_safe_imm_oi_rpc:implementer_set(Handle, IMPL_NAME),
    ct:pal("Set Class Implementer for: ~p", [IMM_TEST_CLASS]),
    ok = rct_safe_imm_oi_rpc:class_implementer_set(Handle, IMM_TEST_CLASS),
    Handle.

%% ===========================================================================
%% @doc
%% Delete Class implementer. <br/>
%% @end
%% ===========================================================================
delete_oi(IMPL_NAME, IMM_TEST_CLASS, Handle) ->
        %%%%
	%% Delete OI for the testfragment
        %%%%
	ct:pal("Release Class Implementer for: ~p", [IMM_TEST_CLASS]),
	ok = rct_safe_imm_oi_rpc:class_implementer_release(Handle, 
							   IMM_TEST_CLASS),
	ct:pal("Clear Implementer ~p", [IMPL_NAME]),
	ok = rct_safe_imm_oi_rpc:implementer_clear(Handle),
	ct:pal("Finalize OI Handle ~p", [Handle]),
	ok = rct_safe_imm_oi_rpc:finalize(Handle),
    ok.

%% ===========================================================================
%% @doc
%% Check that number of expected MOs exist. <br/>
%% @end
%% ===========================================================================
wait_for_nr_of_exp_mo_inst_exist(Cli_Session, NrOfExpMo, PrintOpt) ->
    ok = rct_cli:connect(Cli_Session, PrintOpt),
    wait_for_nr_of_exp_mo_inst_exist(Cli_Session, NrOfExpMo, PrintOpt, 300000).

wait_for_nr_of_exp_mo_inst_exist(Cli_Session, 
				 NrOfExpMo, 
				 PrintOpt, 
				 Timeout) when Timeout < 500 ->
    NrOfCreatedMO = 
	rct_cli_testclass1_lib:get_no_of_created_mo_inst(Cli_Session, 
							 PrintOpt),
    {ok, Recieved_Data} = rct_cli:send(Cli_Session, 
				       "show ManagedElement=1,TestRoot=1", 
				       PrintOpt),
    rct_cli:disconnect(Cli_Session, PrintOpt),

    RecievedData=string:tokens(Recieved_Data, "\r\n> "),
    ct:pal("RecievedData: ~p", [RecievedData]),
    ct:pal("NrOfCreatedMO: ~p does not match NrOfExpMo: ~p.\n",[NrOfCreatedMO, 
								NrOfExpMo]),
    ct:fail("NrOfCreatedMO does not match NrOfExpMo, after create");

wait_for_nr_of_exp_mo_inst_exist(Cli_Session, NrOfExpMo, PrintOpt, Timeout) ->
     NrOfExistingMO = 
	rct_cli_testclass1_lib:get_no_of_created_mo_inst(Cli_Session, 
							 PrintOpt),
    case NrOfExistingMO of
	NrOfExpMo ->
	    ct:log("NrOfExistingMO: ~p match NrOfExpMo: ~p.\n",
		   [NrOfExistingMO, NrOfExpMo]),
	    ok = rct_cli:disconnect(Cli_Session, PrintOpt),
	    ok;
	_Other ->
	    timer:sleep(1000),
	    ct:log("NrOfExistingMO: ~p, NrOfExpMo: ~p", [NrOfExistingMO, 
							 NrOfExpMo]),
	    wait_for_nr_of_exp_mo_inst_exist(Cli_Session, NrOfExpMo, 
					     PrintOpt, Timeout-1000)
    end.

%% ===========================================================================
%% @doc
%% The Time to get all instances under TestRoot, using show cli operation. <br/>
%% @end
%% ===========================================================================
time_to_show_all_under_testroot(Cli_Session, PrintOpt) ->
    ok = rct_cli:connect(Cli_Session, PrintOpt),
    Start = os:timestamp(),
    {ok , _Recieved_Data} = 
	rct_cli:send(Cli_Session, 
		     "show all ManagedElement=1,TestRoot=1", ">",
		     PrintOpt), 
    End = os:timestamp(),
    TimeToReadAll = 
	trunc(timer:now_diff(End, Start) / 1000 /1000),
    ok = rct_cli:disconnect(Cli_Session, PrintOpt),
    TimeToReadAll.

%% ===========================================================================
%% @doc
%% The Time to get one MO instances, using show cli operation. <br/>
%% @end
%% ===========================================================================
time_to_get_one_mo_inst_at_time(Cli_Session, InstNameList, PrintOpt) ->
    ok = rct_cli:connect(Cli_Session, PrintOpt),
    Times = 
	lists:map(
	  fun(InstName) ->
		  Start = os:timestamp(),
		  ok = rct_cli_testclass1_lib:
		      check_mo_inst_created(Cli_Session, InstName, PrintOpt),
		  End = os:timestamp(),
		  TimeToReadMO = trunc(timer:now_diff(End, Start) / 1000),
		  %% ct:pal("### TimeToReadMO: ~p", [TimeToReadMO]),
		  TimeToReadMO
	  end, InstNameList),
    ok = rct_cli:disconnect(Cli_Session, PrintOpt),
    ct:pal("### Times in ms: ~p", [Times]),
    TotTimeToReadMO= lists:foldl(fun(X, Sum) -> 
					 X + Sum 
				 end, 0, Times),
    {TotTimeToReadMO, lists:min(Times), lists:max(Times)}.


%% ===========================================================================
%% @doc
%% Get CXS label. <br/>
%% Get SW version using COM cli interface. <br/>
%% @spec get_sw_version() -> 'CXS_LABEL'
%% @end
%% ===========================================================================
get_sw_version() ->
    [CLI_Name|_T] = ?CLI_SessionNameList,
    CXS_label = rct_tlib:get_sw_version(CLI_Name),
    CXS_label.

%% ===========================================================================
%% @doc
%% check_oi_exist(). <br/>
%% @end
%% ===========================================================================
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

%%% #0.    BASIC INFORMATION
%% coding: latin-1
%%% ----------------------------------------------------------
%%% %CCaseFile:	meas_time_netconf_operation_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R3A/R4A/R5A/2
%%% 
%%% @doc == Measure times when one nc user add, get, delete several mo instances.==
%%% This Test Suite can be used on target and simulated enviroment.<br/>
%%%
%%% 
%%% @end

-module(meas_time_netconf_operation_SUITE).
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
%%% R2A/1      2013-06-26 etxivri     Created
%%% R2A/3      2013-09-19 etxivri     Updates due to renamed functions in rct_nc_testclass1_lib
%%% R2A/4      2013-10-15 etxivri     Updates the TCs to be similar to meas_time_cli
%%% R2A/4      2013-10-18 etxivri     Minor update.
%%% R2A/6      2013-10-22 etxivri     updates due to changes in rct_nc_testclass1_lib.
%%%                                   Updates delete mo inst if TC fail.
%%% R2A/7      2013-10-22 etxivri     Added ##LogDirX as key used for plot
%%% R2A/8      2013-11-08 etxivri     Update check that implementer is created.
%%% R2A/9      2013-11-20 etxivri     Increased NrOfTimes from 5 to 10 to logg, 
%%%                                   in add_del_several_mo_inst_attr_nroftimes
%%% R2A/10      2013-11-20 etxivri    Use of correct LOG_DIR.
%%% R2A/11      2013-11-21 etxivri    Increase nr of intances to be used in
%%%                                   add_del_several_mo_inst_attr_one_commit_and_show_oper
%%%                                   Reduce nr of show repetition from 10 to 3,
%%%                                   due to TC will take long time.
%%%                                   Add measure get all under testroot in one
%%%                                   operation. Remove repetition of get.
%%%                                   Removed check in ets and added check in 
%%%                                   testroot instead.
%%%                                   File with measured data separate with
%%%                                   specific HW.
%%% R2A/13     2013-12-02 etxivri     Correct a printout in a ct:log
%%% R2A/14     2013-12-02 etxivri     Removed a check when set up nc session.
%%% R2A/15     2014-03-28 etxivri     Increased some timeouts.
%%% R2A/16     2014-05-26 etxivri     Update due to new com behaviour.
%%%                                   Several instances could get in differnts
%%%                                   order. Check need to be updated.
%%% R2A/17     2014-09-01 etxivri     Update to add branch on loged file.
%%% R3A/1      2014-12-11 etxivri     Update get sw vversion.
%%% R3A/1      2015-03-20 etxivri     Increasd nr of MOs.
%%% R3A/3      2015-03-30 etxivri     decreased nr of MOs due to it takes to long time.
%%% R3A/4      2015-03-30 etxivri     New trie to increase nr of MOs.
%%% R3A/5      2015-03-31 etxivri     Increased timeouts and decreased use MOs.
%%% R3A/6      2015-04-07 etxivri     Add a simple check of existing MOs.
%%%                                   Decreased nr of MOs,it takes to long time.
%%% R3A/7      2015-04-07 etxivri     Update nr of MOs used in get cases.
%%% R3A/7      2015-07-16 etxivri     Update to be more robust.
%%% R4A/3      2015-09-30 etxivri     Changed now() to os:timestamp().
%%% R5A/1      2015-10-07 etxivri     Update for R5
%%% R5A/2      2016-02-12 etxkols     Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
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

-define(NrOfNetConfUsers, 1).
-define(CLI_Name, cli1). %% CLI hook name. To get SW version

-define(NrOfMoInstList, lists:seq(1, 500) ).
-define(NrOfMoInstList_2, lists:seq(1,500) ).

-define(GetList_500_MO, lists:seq(1, 500) ). %% Used in get.

%% -define(NrOfMoInstList, lists:seq(1,2) ).
%% used in add_del_several_mo_inst_attr_nroftimes
-define(NrOfTimes, 10 ). %% used in add_del_several_mo_inst_attr_nroftimes

%% used in add_del_several_mo_inst_attr_one_commit_and_show_oper
-define(NrOfTimes2, 10).  % 500 inst
%% -define(NrOfTimes2, 20).  % 1000 inst

%% -define(ShowNrOfTimes, 10).

-define(NC_SessionNameList, 
	[list_to_atom(
	   "nc"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfNetConfUsers)]).

-define(IMPL_NAME_1, "ImplementerOne").
-define(IMM_TEST_CLASS1, "TESTMOMTestClass1").

-define(LOG_DIR, "/proj/rcs/measurements/nc_cli_operations/").
%% -define(LOG_DIR, "/home/etxivri/tmp/").

-define(CHECK_SESSION, check_session).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% runs the rct_netconf hook for each user, <br/>
%% to be able to open a netconf session for each user.<br/>
%% @end
%%--------------------------------------------------------------------
suite() -> 
    %% build up a list of NetconfHooks touples with differents Names.
    NetconfHooks = 
	[{rct_netconf, 
	  list_to_atom(
	    "nc"++integer_to_list(N))} || N <- lists:seq(1,?NrOfNetConfUsers)],

    %% ct:pal("NetconfHooks: ~p",[NetconfHooks]),
    %% [{timetrap, {minutes, 600}}, % 10 hours
    [{timetrap, {hours, 7}}, % 7 hours
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 %% {cth_conn_log, []},
		 {rct_power,node},
		 {rct_consserv,cs1},
                 {rct_rs232,console},
		 {rct_cli, {cli1, [manual_connect]}},
		 {rct_core,[]},
		 {rct_netconf, check_session},
		 {rct_safe_rpc,[{safe_services, 
				 [{imm, 0}]}, %% debuglevel 0
				{instance_name, rct_safe_imm_oi_rpc}, 
				{finalize_on_fail, true}]},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT",
						"CRASH REPORT"],[]}}]}} | 
		 %% {rct_logging, {all, [{erlang,{[],[]}}]}} |
		 NetconfHooks
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

	    A = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 
			     10000, noprint),
	    C = rct_rpc:call(rpc, os, cmd, ["pgrep netconf"], 10000, noprint),
	    D = rct_rpc:call(rpc, os, getpid, [], 10000),
    	    ct:pal("### Com: ~p",[A]),
	    ct:pal("### Netconf: ~p",[C]),
	    ct:pal("### BeamPid: ~p .",[D]),

	    OI = rct_rpc:call(rpc, safs_imm_db, ci_get, 
			      ['TESTMOMTestClass1'], 10000, noprint),
	    ct:pal("~p.", [OI])

    end,

    rct_rpc:call(rpc, os, cmd, ["sync"], 30000, noprint),

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
%% Times for one Netconf users, add, delete MO instance and attributes. <br/>
%% Repeat several times. <br/>
%% - Open nc session. <br/>
%%   Start measure,  <br/>
%%   add several MO inst and attributes.<br/>
%%   Commit, <br/>
%% - Close nc session. <br/>
%%   Stop measure.  <br/>
%% - Check MO inst is created. <br/>
%% - Open session. <br/>
%%   Start measure, <br/>
%%   Delete several MO and attributes inst. <br/>
%%   Close session.<br/>
%%   Stop measure. <br/>
%% - Check MO inst is deleted.<br/>
%%
%% - print the operation measure times<br/>
%% @spec add_del_several_mo_inst_attr_nroftimes(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
add_del_several_mo_inst_attr_nroftimes(_Config) ->
    ct:pal("### Times for one Netconf users, add, delete ~p MO "
	   "instances and attributes, commit. Repeat nr of times: ~p.",
	   [length(?NrOfMoInstList_2), ?NrOfTimes]),

    %%%%
    %% Get Beam Pid. Will be used when get memsize.
    %%%%
    BeamPid = rct_rpc:call(rpc, os, getpid, [], 10000),
    ct:pal("BeamPid: ~p .",[BeamPid]),

    %%%%
    %% Get testnode Beam Pids.
    %%%%
    {ok, TestNode} = rct_safe_imm_oi_rpc:get_testnode(rct_safe_imm_oi_rpc),
    TestNodeBeamPid = rpc:call(TestNode, os, getpid, [], 5000),
    ct:pal("TestNodeBeamPid: ~p .",[TestNodeBeamPid]),

    ComPid = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 
			  10000, noprint),
    ct:pal("### Com: ~p",[ComPid]),

    %%%%
    %% Connect a OI for the testfragment
    %%%%
    Handle = create_oi(?IMPL_NAME_1, ?IMM_TEST_CLASS1),

    [NC_session|_T]=?NC_SessionNameList,
    NrOfLoop = lists:seq(1, ?NrOfTimes),

    %%%%
    %% Get nr of TestRoot instances exist before TC start.
    %%%%
    {ok,_} = ct_netconfc:open(NC_session, [{timeout, 60000}]),
    TestRootInstances = 
	rct_nc_testclass1_lib:
	nc_get_all_under_testroot(NC_session),
    NrOfTestRootInst = length(TestRootInstances),
    ok = ct_netconfc:close_session(NC_session, 60000),
    ct:pal("NrOfTestRootInst at start of TC: ~p", [NrOfTestRootInst]),

    try
	ok = check_oi_exist(),

        %%%%
	%% Add Mo inst of testClass1 and attributes.
        %%%%
	ct:pal("Add ~p MO instances and attr, one commit. "
	       "Repeat: ~p times.",[length(?NrOfMoInstList_2), NrOfLoop]),
	AllTimeToAdd = 
	    lists:map(
	      fun(LoopNr) ->
		      {ok,_} = ct_netconfc:open(NC_session, 
						[{timeout, 60000}]),
		      InstNamesList = 
			  lists:map(fun(Nr)->
					    InstName = 
						atom_to_list(NC_session)++
						"_"++integer_to_list(LoopNr)++
						"_"++integer_to_list(Nr),
					    InstName
				    end, ?NrOfMoInstList_2),
		      AttrName = integer_to_list(LoopNr),
		      Start = os:timestamp(),
		      lists:foreach(
			fun(InstName) ->
				rct_nc_testclass1_lib:
				    nc_add_mo_instance_and_attr_no_check(
				      NC_session, 
				      InstName,
				      AttrName)
			end, InstNamesList),
		      ok = ct_netconfc:close_session(NC_session, 7200000),
		      End = os:timestamp(),
		      TimeToAdd = 
			  trunc(timer:now_diff(End, Start) / 1000 / 1000),
                     
                      %%%%
		      %% Check Nr of MO instances exist in TestRoot.
                      %%%%
		      %% ExpNrOfTestRootInstAdd =
		      %% 	  NrOfTestRootInst + (LoopNr * 
		      %% 				  length(?NrOfMoInstList_2)),
		      CheckAddStart= os:timestamp(),
		      %% Use this when ct_netconfc xml parser is faster.
		      %% ok = wait_for_expected_inst_exist(ExpNrOfTestRootInstAdd),
		      CheckAddEnd = os:timestamp(),

                      %%%%
		      %% Check MO instances and attributes is created.
                      %%%%
		      {ok,_} = ct_netconfc:open(NC_session, 
		      				[{timeout, 60000}]),
		      lists:foreach(
		      	fun(InstName) ->
		      		ok = 
		      		    rct_nc_testclass1_lib:
		      		    nc_get_attribute_check(NC_session, 
		      					   InstName,
		      					   AttrName)
		      	end, InstNamesList),
		      ok = ct_netconfc:close_session(NC_session, 7200000),

		      
		      TimeToMoInstExist = 
		      	  trunc(timer:now_diff(CheckAddEnd, 
		      			       CheckAddStart) / 1000 / 1000),
		      {TimeToAdd, TimeToMoInstExist}
	      end, 
	      NrOfLoop),
	
	TotAllTimeToAdd = 
	    lists:foldl(fun({TimeToAdd, _TimeToMoInstExist}, Sum) -> 
	    			TimeToAdd + Sum
	    		end, 0, AllTimeToAdd),
	
	rct_rpc:call(rpc, os, cmd, ["sync"], 30000, noprint),
	
        %%%%
	%% Delete MO instances.
        %%%%
	ct:pal("Delete ~p MO instances, one commit. Repeat: ~p times.",
	       [length(?NrOfMoInstList_2), NrOfLoop]),
	AllTimeToDel = 
	    lists:map(
	      fun(LoopNr) ->
		      {ok,_} = ct_netconfc:open(NC_session, 
						[{timeout, 60000}]),
		      InstNamesList = 
			  lists:map(
			    fun(Nr) ->
				    InstName = atom_to_list(NC_session)++
					"_"++integer_to_list(LoopNr)++
					"_"++integer_to_list(Nr),
				    InstName
			    end, ?NrOfMoInstList_2),
		      Start = os:timestamp(),
		      lists:foreach(
			fun(InstName) ->			      
				rct_nc_testclass1_lib:
				    nc_delete_mo_instance_no_check(NC_session, 
								   InstName)
			end, InstNamesList),
		      ok = ct_netconfc:close_session(NC_session, 7200000),
		      End = os:timestamp(),
		      TimeToDel = 
			  trunc(timer:now_diff(End, Start) / 1000 / 1000),
                      
                      %%%%
		      %% Check Nr of MO instances deleted in TestRoot.
                      %%%%
		      %% ExpNrOfTestRootInstDel = NrOfTestRootInst +
		      %% 				 ((?NrOfTimes+1 - LoopNr) * 
		      %% 				      length(?NrOfMoInstList_2))
		      %% 			     - length(InstNamesList),
		      CheckDelStart = os:timestamp(),
		      %% Use this when ct_netconfc xml parser is faster.
		      %% ok = wait_for_expected_inst_exist(ExpNrOfTestRootInstDel),
		      CheckDelEnd = os:timestamp(),

                      %%%%
		      %% Check MO instance deleted.
                      %%%%
		      {ok,_} = ct_netconfc:open(NC_session, 
						[{timeout, 60000}]),
		      lists:foreach(
			fun(InstName) ->
				ok = 
				    rct_nc_testclass1_lib:
				    nc_check_mo_instance_deleted(NC_session, 
								 InstName)
			end, InstNamesList),
		      TimeToMoInstDel = 
		      	  trunc(timer:now_diff(CheckDelEnd, 
		      			       CheckDelStart) / 1000 / 1000),
		      ok = ct_netconfc:close_session(NC_session, 60000),
		      {TimeToDel, TimeToMoInstDel}
	      end, 
	      NrOfLoop),
	
	TotAllTimeToDel = 
	    lists:foldl(fun({TimeToDel, _TimeToMoInstDel}, Sum) -> 
	    			TimeToDel + Sum
	    		end, 0, AllTimeToDel),
	
	All_TimeToAdd = 
	    lists:map(fun({TimeToAdd, _TimeToMoInstExist}) ->
			      TimeToAdd
		      end, AllTimeToAdd),
	All_TimeToDel = 
	    lists:map(fun({TimeToDel, _TimeToMoInstDel}) ->
			      TimeToDel
		      end, AllTimeToDel),

	%%%%
	%% Write info to file
        %%%%
	CXS_label = get_sw_version(),
	[{_, NodeName}] = ct:get_config(test_nodes),
	%% FileName =
	%%     get_filename("nc_add_delete_several_mo_inst_attr_nroftimes"),
	BoardType = meas_lib:get_board_type(),
	Branch = meas_lib:get_branch(),
	FileName = Branch++"_"++BoardType++"_nc_add_delete_several_mo_inst_attr_nroftimes",
	rct_tlib:writeDataToFile(?LOG_DIR, 
				 FileName, 
				 "~p;~w;~p;~p;"
				 "~p;~p;~p;~p;~p;~p;~p;~p;~p;~p;"
				 "~p;~p;~p;~p;~p;~p;~p;~p;~p;~p;"
				 "~w~n", 
				 [httpd_util:rfc1123_date(),
				  CXS_label,
				  length(?NrOfMoInstList_2),
				  ?NrOfTimes]++
				     All_TimeToAdd++
				     %% [TotAllTimeToAdd]++
				     All_TimeToDel++ 
				     %% [TotAllTimeToDel]++
				     [NodeName]
				),

	%%%%
	%% Print out some results.
        %%%%
	ZipTimes = lists:zip(AllTimeToAdd, AllTimeToDel),
	lists:foreach(
	  fun({{TimeToAdd, _TimeToMoInstExist}, 
	       {TimeToDel, _TimeToMoInstDel}}) ->
		  ct:pal("### TimeToAdd: ~p sec, TimeToMoInstExist: ~p sec,  "
			 "TimeToDel: ~p sec, TimeToMoInstDel: ~p sec.",
			 [TimeToAdd, "not checked", 
			  TimeToDel, "not checked"])
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
    	_:Reason -> %% Case TC fail, CleanUp and then delete OI.
	    lists:foreach(
	      fun(LoopNr) ->
		      InstNamesList = 
			  lists:map(
			    fun(Nr) ->
				    InstName = atom_to_list(NC_session)++
					"_"++integer_to_list(LoopNr)++
					"_"++integer_to_list(Nr),
				    InstName
			    end, ?NrOfMoInstList_2),
		      lists:foreach(
			fun(InstName) ->	
				ct_netconfc:open(NC_session, [{timeout,500}]),
				rct_nc_testclass1_lib:
				    nc_delete_mo_instance_no_check(NC_session, 
								   InstName),
				ct_netconfc:close_session(NC_session, 500)
			end, InstNamesList)
	      end, 
	      NrOfLoop),
            %%%%
    	    %% Delete OI for the testfragment
            %%%%
    	    ct:pal("TC will fail, Reason:~p",[Reason]),
    	    ok = delete_oi(?IMPL_NAME_1, ?IMM_TEST_CLASS1, Handle),
	    ct:fail(Reason)
    end,
	
    ok.

%% ok = ct_netconfc:close_session(?CHECK_SESSION),
%%--------------------------------------------------------------------
%% @doc
%% Times for one Netconf users, add, get, delete several MO instances. <br/>
%% Do commit after each operation is done. <br/>
%% - Add MO instances and attr, No commit.<br/>
%% - Commit.<br/>
%% - Get all MOs.<br/>
%% - Delete all MO instances, No commit<br/>
%% - Commit.<br/>
%% - print the operation times<br/>
%% @spec add_del_several_mo_inst_attr_one_commit_and_show_oper(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
add_del_several_mo_inst_attr_one_commit_and_show_oper(_Config) ->
    ct:pal("### Times for one NC user, add, delete ~p MO instances and "
	   "attributes. One Commit, Measure also get operation!",
	   [?NrOfTimes2*length(?NrOfMoInstList)]),

    %%%%
    %% Get Com pid.
    %%%%
    ComPid = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 
			  10000, noprint),
    ct:pal("### Com: ~p",[ComPid]),

    %%%%
    %% Create a OI for the testfragment
    %%%%
    Handle = create_oi(?IMPL_NAME_1, ?IMM_TEST_CLASS1),

    [NC_session|_T]=?NC_SessionNameList,
    NrOfMoInst = ?NrOfTimes2 * length(?NrOfMoInstList),
    NrOfMoInst_List = lists:seq(1, NrOfMoInst),

    %%%%
    %% Get nr of TestRoot instances exist before TC start.
    %%%%
    {ok,_} = ct_netconfc:open(NC_session, [{timeout, 60000}]),
    TestRootInstances = 
	rct_nc_testclass1_lib:
	nc_get_all_under_testroot(NC_session),
    NrOfTestRootInst = length(TestRootInstances),
    ok = ct_netconfc:close_session(NC_session, 60000),
    ct:pal("NrOfTestRootInst at start of TC: ~p", [NrOfTestRootInst]),

    try
	ok = check_oi_exist(),

        %%%%
	%% Add Mo inst of testClass1 and attributes
        %%%%
	ct:pal("Add ~p MO instances and attr in one operation.",
	       [length(NrOfMoInst_List)]),
	{ok,_} = ct_netconfc:open(NC_session, [{timeout, 60000}]),
	AddStart = os:timestamp(),
	ok = rct_nc_testclass1_lib:
	    nc_add_nr_of_mo_instance_and_attr(NC_session, 
					      NrOfMoInst_List),
	AddEnd = os:timestamp(),
	ok = ct_netconfc:close_session(NC_session, 7200000),
	AddCommitEnd = os:timestamp(),

	ct:pal("### Add done. Start Check ###", []),

        %%%%
	%% Check Nr of MO instances exist in TestRoot.
        %%%%
	ExpNrOfTestRootInstAdd =
	    NrOfTestRootInst + (?NrOfTimes2 * length(?NrOfMoInstList)),
	CheckAddStart= os:timestamp(),
	ok = wait_for_expected_inst_exist(ExpNrOfTestRootInstAdd),
	CheckAddEnd = os:timestamp(),


	rct_rpc:call(rpc, os, cmd, ["sync"], 30000, noprint),
        %%%%
	%% Some time calculataion
        %%%%
	TimeToAdd = trunc(timer:now_diff(AddEnd, AddStart) / 1000 / 1000),
	CommitAddEnd = 
	    trunc(timer:now_diff(AddCommitEnd, AddStart) / 1000 / 1000),
	ct:pal("TimeToaDD: ~p MO inst and attr in one operation: ~p sec, "
	       "commit after all is added. Commit end: ~p .",[NrOfMoInst,
							      TimeToAdd, 
							      CommitAddEnd]),

	TimeToMoInstExist = 
	    trunc(timer:now_diff(CheckAddEnd, CheckAddStart) / 1000 / 1000),
	ct:pal("TimeToMoInstExist: ~p sec, ",[TimeToMoInstExist]),


     	%%%%%%%%%%%%%%%%%%%%%%%
	%% Netconf get operation
	%%%%%%%%%%%%%%%%%%%%%%%
	%%%%
	%% "Get everything under TestRoot in one operation
	%%%%
	ct:pal("Get everything under TestRoot in one operation, ", []),
	TimeToGetTestRoot = 
	    get_all_under_testroot(NC_session, NrOfMoInst_List),
	ct:log("TimeToGetTestRoot: ~p",[TimeToGetTestRoot]),

	%% Get wilk use 500 MOs due to it takes long time and it is not 
	%% relevant to get more. 
	GetNrOfMOs = length(?GetList_500_MO),
	%%%%
	%% Get every created testclass1 instance in one operation
	%%%%
	ct:pal("Get ~p created testclass1 instance in one operation", 
	       [GetNrOfMOs]),
	TimeToReadAllTestClass1 = 
	    time_to_read_all_mo_in_one_get(NC_session, ?GetList_500_MO),

	%%%%
	%% Get every created testclass1 instance one by one
	%%%%
	ct:pal("Get ~p created MO, one by one.", [GetNrOfMOs]),
	{Tot_TimeToReadMO, Min_TimeToReadMO, Max_TimeToReadMO} = 
	    time_to_get_one_mo_inst_at_time(NC_session, ?GetList_500_MO), 
	Avg_TimeToReadMO = round(Tot_TimeToReadMO/length(?GetList_500_MO)),
	Tot_TimeToReadMO_In_sec = round(Tot_TimeToReadMO/1000),
	
        %%%%
	%% Delete MO instances
        %%%%
	ct:pal("Delete ~p MO instances.",[NrOfMoInst]),
	{ok,_} = ct_netconfc:open(NC_session, [{timeout, 60000}]),
	StartDel = os:timestamp(),
	ok = rct_nc_testclass1_lib:nc_del_nr_of_mo_instance(NC_session,
							    NrOfMoInst_List),
	EndDel = os:timestamp(),
	ok = ct_netconfc:close_session(NC_session, 7200000),
	DelCommitEnd = os:timestamp(),

	ct:pal("### Delete done. Start Check ###", []),
 
        %%%%
	%% Check Nr of MO instances deleted in TestRoot.
        %%%%
	CheckDelStart = os:timestamp(),
	ok = wait_for_expected_inst_exist(NrOfTestRootInst),
	CheckDelEnd = os:timestamp(),

	rct_rpc:call(rpc, os, cmd, ["sync"], 30000, noprint),
        %%%%
	%% Some time calculataion
        %%%%
	TimeToDel = trunc(timer:now_diff(EndDel, StartDel) / 1000 / 1000),
	CommitDelEnd = 
	    trunc(timer:now_diff(DelCommitEnd, StartDel) / 1000 / 1000),
	ct:pal("TimeToDel: ~p MO inst: ~p sec, "
	       "commit after all is deleted. Commit end: ~p .",[NrOfMoInst,
								TimeToDel, 
								CommitDelEnd]),

	TimeToMoInstDel = 
	    trunc(timer:now_diff(CheckDelEnd, CheckDelStart) / 1000 / 1000),
	ct:pal("TimeToMoInstDel: ~p sec, ",[TimeToMoInstDel]),

	%%%%
	%% Check MO instance deleted.
        %%%%
	{ok,_} = ct_netconfc:open(NC_session, [{timeout, 60000}]),
	lists:foreach(
	  fun(Nr3) ->
		  InstName = 
		      atom_to_list(NC_session)++"_"++integer_to_list(Nr3),
		  ok = rct_nc_testclass1_lib:
		      nc_check_mo_instance_deleted(NC_session, 
						   InstName)
	  end, 
	  ?NrOfMoInstList),
	ok = ct_netconfc:close_session(NC_session, 60000),

	%%%%
	%% Write info to file
        %%%%
	CXS_label = get_sw_version(),
	[{_, NodeName}] = ct:get_config(test_nodes),
	%% FileName = 
	%%     get_filename("nc_add_del_several_mo_inst_attr_one_commit"),
	BoardType = meas_lib:get_board_type(),
	Branch = meas_lib:get_branch(),
	FileName = Branch++"_"++BoardType++"_nc_add_del_several_mo_inst_attr_one_commit",
	rct_tlib:writeDataToFile(?LOG_DIR, 
				 FileName, 
				 "~p;~w;~p;~p;~p;~p;~p;~p;~p;~w~n", 
				 [httpd_util:rfc1123_date(),
				  CXS_label,
				  length(NrOfMoInst_List),
				  TimeToAdd,
				  CommitAddEnd,
				  TimeToMoInstExist,
				  TimeToDel,
				  CommitDelEnd,
				  TimeToMoInstDel,
				  NodeName]
				),

	%% FileName2 = 
	%%     get_filename("nc_show_operation_several_mo_inst"),
	FileName2 = Branch++"_"++BoardType++"_nc_show_operation_several_mo_inst",
	rct_tlib:writeDataToFile(?LOG_DIR, 
				 FileName2, 
				 "~p;~w;~p;~p;~p;~p;~p;~p;~p;~w~n", 
				 [httpd_util:rfc1123_date(),
				  CXS_label,
				  length(NrOfMoInst_List),
				  TimeToGetTestRoot,
				  TimeToReadAllTestClass1,
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

	ct:pal("Get all created MOs at in one rperation, "
	       "TimeToGetTestRoot: ~p sec, "
	       "TimeToReadAllTestClass1: ~p sec.",
	       [%% RepeatNr,
		TimeToGetTestRoot,
		TimeToReadAllTestClass1
	       ]),
	
	ct:pal("Get ~p created MO, one by one, tot: ~p sec, avg: ~p ms, "
	       "min: ~p ms, max: ~p ms.",[length(NrOfMoInst_List), 
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
	    lists:foreach(
	      fun(Nr) ->	
		      InstName = 
			  atom_to_list(NC_session)++"_"++integer_to_list(Nr),
		      ct_netconfc:open(NC_session, [{timeout,500}]),
		      rct_nc_testclass1_lib:
			  nc_delete_mo_instance_no_check(NC_session, 
							 InstName),
		      ct_netconfc:close_session(NC_session, 500)
	      end, NrOfMoInst_List),
	    
    	    ok = delete_oi(?IMPL_NAME_1, ?IMM_TEST_CLASS1, Handle),
	    ct:fail(Reason)
    end,

    ok.


%%% Internal functions
%% ===========================================================================
%% @doc
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
%% wait for nr of expected intances exist in testroot. <br/>
%% @end
%% ===========================================================================
wait_for_expected_inst_exist(ExpNrOfTestRootIns)->
    ct:log("¤¤ ~p",[ExpNrOfTestRootIns]),
    wait_for_expected_inst_exist(ExpNrOfTestRootIns, 60000).
wait_for_expected_inst_exist(_ExpNrOfTestRootIns, Timeout) when Timeout < 0 -> 
    ct_netconfc:close_session(?CHECK_SESSION, 60000),
    ct:fail("Nr of Expected MO inst does Not exist within expected time.");
wait_for_expected_inst_exist(ExpNrOfTestRootIns, Timeout) ->
    ct_netconfc:open(?CHECK_SESSION, [{timeout, 60000}]),
    TestRootInst = rct_nc_testclass1_lib:
	nc_get_all_under_testroot(?CHECK_SESSION),
    %% ct:pal("¤¤ ~p",[ExpNrOfTestRootIns]),
    %% ct:pal("¤¤ ~p",[length(TestRootInst)]),

    case length(TestRootInst) of
	ExpNrOfTestRootIns ->
	    ct:log("## ExpNrOfTestRootIns: ~p", [ExpNrOfTestRootIns]),
	    ok = ct_netconfc:close_session(?CHECK_SESSION, 60000),
	    ok;
	_Res ->
	    ct:log("## ~p , Not expected ExpNrOfTestRootIns: ~p", 
		       [_Res, ExpNrOfTestRootIns]),
	    timer:sleep(1000),
	    wait_for_expected_inst_exist(ExpNrOfTestRootIns, Timeout-1000)
    end.

%% ===========================================================================
%% @doc
%% The Time to get all testclass1, using one get nc operation. <br/>
%% @end
%% ===========================================================================
%% time_to_read_all_nroftimes(NC_session, NrOfMoInstList) ->
time_to_read_all_mo_in_one_get(NC_session, NrOfMoInstList) ->
    {ok,_} = ct_netconfc:open(NC_session, [{timeout, 60000}]),
    Start = os:timestamp(),
    All_Instances_Attr = 
	rct_nc_testclass1_lib:
	nc_get_several_mo_inst_check_inst_and_attr(
	  NC_session, NrOfMoInstList),
    End = os:timestamp(),
    ct:log("### Close  session after get several testclass1 inst and attr"),
    ok = ct_netconfc:close_session(NC_session, 7200000),
    ct:log("All_Instances_Attr: ~p",[All_Instances_Attr]),

    %% ct:log("### check all testclass1 exist, start"),
    %% check_all_expected_attr(All_Instances_Attr, 
    %% 			    NrOfMoInstList, 
    %% 			    NC_session),
    %% ct:log("### check all testclass1 exist, done"),

    ct:log("### simple check that expected nr of MOs exist. Start"),
    A = length(NrOfMoInstList),
    B = length(All_Instances_Attr),
    ct:log("### Nr of Exp MOs: ~p. Nr of existing MOs: ~p", [A, B]),
    A = B,
    ct:log("### simple check that expected nr of MOs exist. Done"),

    TimeToReadAll = 
	trunc(timer:now_diff(End, Start) / 1000 / 1000),
    ct:pal("Time to Get testclass1 MOs in one operation: ~p sec, ", 
	   [TimeToReadAll]),
    TimeToReadAll.

%% check_all_expected_attr(All_Instances_Attr, NrOfMoInstList, NC_session) ->
%%     ZipList = lists:zip(All_Instances_Attr, NrOfMoInstList),
%%     lists:foreach(
%%       fun({TestClass1List, Nr}) ->
%% 	      InstName = atom_to_list(NC_session)++"_"++integer_to_list(Nr),
%% 	      AttrName = integer_to_list(Nr),
%% 	      {'TestClass1',[], [{testClass1Id,[],[InstName]},
%% 				 {struct1,
%% 				  [{struct,"Struct1"}],
%% 				  AttrStructList},
%% 				 {int32,[],[AttrName]}]} = TestClass1List,
	      
%% 	      true = lists:member({struct1mem1,[],[AttrName]}, AttrStructList),
%% 	      true = lists:member({struct1mem2,[],["init"]}, AttrStructList)
%%       end, 
%%       ZipList),
%%     ok.

%% ===========================================================================
%% @doc
%% The Time to get all instances under TestRoot, using get nc operation. <br/>
%% @end
%% ===========================================================================
get_all_under_testroot(NC_session, NrOfMoInst_List) ->
    {ok,_} = ct_netconfc:open(NC_session, [{timeout, 60000}]),
    GetTestRootStart = os:timestamp(),
    AllInstancesAttr =
	rct_nc_testclass1_lib:nc_get_all_under_testroot(NC_session),
    %% ct:pal("~p", [AllInstancesAttr]),
    GetTestRootEnd = os:timestamp(),
    ct:log("### Close  session after get all under testroot."),
    ok = ct_netconfc:close_session(NC_session, 7200000),
    ct:log("### Check expected inst and attr exist, start."),
    check_expected_inst_attr(AllInstancesAttr, 
			     NrOfMoInst_List, 
			     NC_session),
    ct:log("###  Check expected inst and attr exist, done."),
    TimeToGetTestRoot = 
	trunc(timer:now_diff(GetTestRootEnd, 
			     GetTestRootStart) / 1000 / 1000),
    TimeToGetTestRoot.

check_expected_inst_attr(All_Instances_Attr, NrOfMoInstList, NC_session) ->
    lists:foreach(
      fun(Nr) ->
	      InstName = atom_to_list(NC_session)++"_"++integer_to_list(Nr),
	      AttrName = integer_to_list(Nr),
	      
	      lists:keyfind({'TestClass1',[], [{testClass1Id,[],[InstName]},
					       {struct1,
						[{struct,"Struct1"}],
						[{struct1mem1,[],[AttrName]},
						 {struct1mem2,[],["init"]}]},
					       {int32,[],[AttrName]}]}, 
			    1, 
			    All_Instances_Attr)
      end, 
      NrOfMoInstList),
    ok.

%% ===========================================================================
%% @doc
%% The Time to get one MO instances, using show nc operation. <br/>
%% @end
%% ===========================================================================
time_to_get_one_mo_inst_at_time(NC_session, NrOfMoInstList) ->
    {ok,_} = ct_netconfc:open(NC_session, [{timeout, 60000}]),
        ct:pal("### Start get one by one"),

    Times = 
	lists:map(
	  fun(Nr) ->
		  InstName = atom_to_list(NC_session)++
		      "_"++integer_to_list(Nr),
		  Start = os:timestamp(),
		  ok = rct_nc_testclass1_lib:
		      nc_get_mo_instance_check(NC_session, InstName),
		  End = os:timestamp(),
		  TimeToReadMO = trunc(timer:now_diff(End, Start) / 1000),
		  %% ct:pal("### TimeToReadMO: ~p", [TimeToReadMO]),
		  TimeToReadMO
	  end, NrOfMoInstList),
    ok = ct_netconfc:close_session(NC_session, 60000),
    ct:log("### Close  session after get one by one"),
    
    ct:log("### All times to get mo inst one at time: ~w", [Times]),
    TotTimeToReadMO= lists:foldl(fun(X, Sum) -> 
					 X + Sum 
				  end, 0, Times),
    {TotTimeToReadMO, lists:min(Times), lists:max(Times)}.


%% ===========================================================================
%% @doc
%% check_oi_exist()
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

%% ===========================================================================
%% @doc
%% Get CXS label. <br/>
%% Get SW version using COM cli interface. <br/>
%% @spec get_sw_version() -> 'CXS_LABEL'
%% @end
%% ===========================================================================
get_sw_version() ->
    CXS_label = rct_tlib:get_sw_version(?CLI_Name),
    CXS_label.

%% %% ===========================================================================
%% %% @hidden
%% %% ===========================================================================
%% get_filename(FileNameStr) ->
%%     ct:pal("### FileNameStr : ~p",[FileNameStr]),
%%     FileName = rct_tlib:get_filename(FileNameStr),
%%     FileName.

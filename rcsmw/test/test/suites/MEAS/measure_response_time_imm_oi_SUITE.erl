%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	measure_response_time_imm_oi_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R3A/R4A/R5A/R6A/2
%%% 
%%% @doc ==Measure responce times for differents netconf operation on a fragment that belongs to application that is Object Implementor.==
%%% This Test Suite can be used on target enviroment.
%%% Used sw version and measured data is written to a file.
%%% Path: /proj/rcs/measurements/
%%%
%%% <br/><br/>
%%% 
%%% @end


-module(measure_response_time_imm_oi_SUITE).
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
%%% R2A/2      2013-04-02 etxivri     Created
%%%                                   Note! this will not work if you don't load safe to target and give correct path.
%%%                                   Or if safe wil automatically be loaded on target? and started ?
%%%                                   Note! cpulimit need to be coopied to dev_patches.
%%%                                   (This is not ready to be used!, Missing SAFE and handle callbacks from IMM OI.)
%%% R2A/3      2013-04-12 etxivri     Still not ready to be used! Missing safe to be loaded and cpulimit in vob.
%%% R2A/4      2013-04-30 etxivri     Changed timeout value in rct_scp
%%% R2A/5      2013-05-20 etxivri     Updated cleanu in end per tc if fail.
%%% R2A/6      2013-05-20 etxivri     Forgot to fill in revision log for /5.
%%% R2A/7      2013-06-04 etxivri     Some updates, in cleanup when fail, and a check when netconf get.
%%% R2A/8      2013-09-10 etxivri     Updates due to renamed saf-rpc hooks.
%%% R2A/8      2013-09-19 etxivri     Updates due use of rct_nc_testclass1_lib.
%%% R2A/10     2013-11-07 etxivri     Updates to suport cpulimit on ARM,
%%% R2A/11     2013-11-21 etxivri     Added a tag of measured file to separate 
%%%                                   presentation between dus,duw2 and tcu03.
%%% R2A/12     2014-01-16 etxkols     Support for dus5201. 
%%% R2A/13     2014-02-25 etxivri     minor update to make it more robust.
%%% R2A/14     2014-02-27 etxivri     Update to make sure pkill works!
%%% R2A/15     2014-02-27 etxivri     Added revision to the measured file.
%%%                                   And use of rct_tlib for write to file.
%%% R2A/16     2014-03-06 etxivri     Use Branch instead of Ver to add in filename.
%%%                                   Changed to /usr/bin/pkill instead of pkill.
%%% R2A/17     2014-03-07 etxivri     Added more printouts if pkill not found.
%%% R3A/1      2015-04-10 etxivri     Decreased cpu load check.
%%% R3A/2      2015-04-22 etxivri     Decreased cpu load check.
%%% R3A/3      2015-04-30 etxivri     Increased timetrap.
%%% R4A/1      2015-09-07 etxivri     Create a new TC specific for tory
%%% R4A/2      2015-09-07 etxivri     Forgett to uncomment rct_core hook.
%%% R4A/3      2015-09-30 etxivri     Changed now() to os:timestamp().
%%% R4A/4      2015-10-01 etxmlar     Allow 2 CPU:s to have lower background load
%%%                                   than expected.
%%% R5A/2      2016-01-12 etxivri     Use median value instead for max value.
%%% R6A/1      2016-05-23 etxivri     Update boardtype if dus5201 is used as bpu
%%% R6A/2      2016-05-26 etxivri     Minor bugfix.
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
	 measure_response_time_nc_operation_imm_oi/1,
	 meas_100_create/1
	]).

-define(NC_Name, nc1). %% NC hook name
-define(CLI_Name, cli1). %% CLI hook name
-define(NrOfInstance, 100).
%% -define(NrOfInstance, 2).
-define(Median, 50).

-define(RESULTDIR, "/proj/rcs/measurements/response_time/").
%% -define(RESULTDIR, "/home/etxivri/tmp/").

%% -define(CPULIMIT_FROM_PATH, "/home/etxivri/Kista/rcs_ithub/cpulimit/git_hub/src_merged_11/cpulimit").
-define(CPULIMIT_FROM_PATH, "/proj/rcs/misc/cpulimit").
-define(CPULIMIT_FROM_PATH_ARM, " /proj/rcs/misc/cpulimit_arm/cpulimit").
-define(CPULIMIT_TO_PATH, "/home/sirpa/dev_patches/").

-define(IMPL_NAME_1, "ImplementerOne").
-define(IMM_TEST_CLASS1, "TESTMOMTestClass1").
-define(INSTANCE_NAME, instance_1).


-define(RESULTDIR_Test, "/proj/rcs/measurements/tory/meas_6").

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{timetrap, {minutes, 660}}, % 11 hours
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {rct_netconf, nc1},
		 %% {cth_conn_log, []},
		 {rct_consserv,cs1},
		 {rct_logging, 
		  {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
                 {rct_rs232,console},
		 {rct_cli, {cli1, [manual_connect]}},
		 %% {safe_oi_dummy_hook,[]}, %%replace by afe_imm_oi_rpc hook.
		 %% {rct_safe_imm_oi_rpc,[{finalize_on_fail, true}]},
		 {rct_safe_imm_oi_rpc,
		  [ {targ_du_no, 1},{instance_name, ?INSTANCE_NAME},
		    {finalize_on_fail, true}]},
		 {rct_tlib,{load,[]}},
		 {rct_scp, [{1, node1}]},
                 {rct_core,[]}
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
init_per_testcase(meas_100_create, Config) ->
    Config;
init_per_testcase(_TestCase, Config) ->
    %%%%
    %% Connect a OI for the testfragment
    %%%%
    ct:pal("Initialize OI Handle", []),
    {ok, Handle, Vsn} = rct_safe_imm_oi_rpc:initialize_2(?INSTANCE_NAME, ok),
    ct:pal("Initialize ok: ~p ~p",[Handle, Vsn]),
    ct:pal("Set Implementer name: ~p", [?IMPL_NAME_1]),
    ok = rct_safe_imm_oi_rpc:implementer_set(?INSTANCE_NAME, Handle, 
					     ?IMPL_NAME_1),
    ct:pal("Set Class Implementer for: ~p", [?IMM_TEST_CLASS1]),
    ok = rct_safe_imm_oi_rpc:class_implementer_set(?INSTANCE_NAME, Handle, 
						   ?IMM_TEST_CLASS1),
    
    [{oi_handle, Handle} | Config].


%% @hidden
end_per_testcase(meas_100_create, Config) ->  
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
  	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p.  \nClean up added TestRoot "
		   "instances.", [Reason]),
	    try
	    	nc_open(ct_netconfc:open(?NC_Name, []), ?NC_Name)
	    catch throw:{?MODULE, found} ->
	    	    ok
	    end,
	    ct_netconfc:close_session(?NC_Name)
    end;
end_per_testcase(_TestCase, Config) ->
    %%%%
    %% Release implementer.
    %%%%
    Handle = ?config(oi_handle, Config),
    ct:pal("Release Class Implementer for: ~p", [?IMM_TEST_CLASS1]),
    ok = rct_safe_imm_oi_rpc:class_implementer_release(?INSTANCE_NAME, Handle, 
						       ?IMM_TEST_CLASS1),
    ct:pal("Clear Implementer ~p", [?IMPL_NAME_1]),
    ok = rct_safe_imm_oi_rpc:implementer_clear(?INSTANCE_NAME, Handle),
    ct:pal("Finalize OI Handle ~p", [Handle]),
    ok = rct_safe_imm_oi_rpc:finalize(?INSTANCE_NAME, Handle),

    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p.  \nClean up added TestRoot "
		   "instances.", [Reason]),
	    %% test_server:break("END per TC , break"),
	   

	    %%%%%%%%%%%%%%
	    %% Clean up Netconf configurations!
	    %%%%%%%%%%%%%%
	    %%%%
	    %% Open a netconf session and clean up rbsUnits.
            %%%%
	    try
	    	nc_open(ct_netconfc:open(?NC_Name, []), ?NC_Name)
	    catch throw:{?MODULE, found} ->
	    	    ok
	    end,

            %%%%
	    %% Commit
            %%%%
	    ct_netconfc:close_session(?NC_Name),
    %% end,

	    [] = rct_rpc:call(rpc, os, cmd, ["/usr/bin/pkill cat"], 10000, noprint),
	    [] = rct_rpc:call(rpc, os, cmd, 
			      ["rm "++?CPULIMIT_TO_PATH++"cpulimit"], 
			      10000, noprint),
	    
	    [] = rct_rpc:call(rpc, os, cmd, ["pgrep cat"], 10000, noprint),
	    [] = rct_rpc:call(rpc, os, cmd, ["pgrep cpulimit"], 10000, noprint),
	    
	    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 5000, noprint)
    end,

    ok.


nc_open({error,_}, Name) ->
    %% Clean up at our client, if netconf process was killed on node, 
    ct_netconfc:close_session(Name), 
    ct_netconfc:open(Name, []), % Then set up again.
    %% nc_delete_rbsunit_no_check(Name),
    NrList = lists:seq(1,?NrOfInstance),
    delete_mo_no_check(Name, NrList),
    throw({?MODULE, found});
nc_open({ok,_}, Name) ->
    %% nc_delete_rbsunit_no_check(Name),
    NrList = lists:seq(1,?NrOfInstance),
    delete_mo_no_check(Name, NrList),
    throw({?MODULE, found});
nc_open(_, _) ->
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


%% ===========================================================================
%% @doc
%% This will do netconf operationa and measure the response time. <br/>
%% Fragment shall be owened by an application <br/>
%% The measured data will be stored in file. <br/>
%% - Time to crate 10 MO instance, with attribute. <br/>
%% - Time to read 100 MO instance, one by one.   <br/>
%% - Time to read 100 MO instance, all together at same time.   <br/>
%% - Generate 50% load on all CPUs. <br/>
%% - Time to create 1 MO instance.  <br/>
%% - Time to add 2  attributes to 1 MO.  <br/>
%% - Time to read 2 attributes from 1 MO.  <br/>
%% - Time to change 1 attributes for 1 MO.  <br/>
%% - Time to delete 1 MO.  <br/>
%% - Time to create 100 MO instance.  <br/>
%% - Time to add 2  attributes to 100 MO.  <br/>
%% - Time to read 2 attributes from 100 MO.  <br/>
%% - Time to change 1 attributes for 100 MO.  <br/>
%% - Time to delete 100 MO.  <br/>
%% - <br/>
%% @spec measure_response_time_nc_operation_imm_oi(_Config) -> ok
%% @end
%% ===========================================================================
measure_response_time_nc_operation_imm_oi(_Config) ->
    SessionName = ?NC_Name,

    [{_, NodeName}] = ct:get_config(test_nodes),

    %%%%
    %% Check OI exist, check for ImplementerOne,
    %%%%
    case rct_rpc:call(rpc, safs_imm_db, ci_get, ['TESTMOMTestClass1'], 10000, 
		      noprint) of
	{_,<<"ImplementerOne">>} ->
	    ok;
	{_,<<"ImplementerOne">>, _} ->
	    ok;
	Ans ->
	    ct:pal("~p", [Ans]),
	    ct:fail(" Expecting OI does not exist! ")
    end,
    %% test_server:break(" break"),

    %%%%
    %% 1. Time to crate 10 MO instance, with attribute,
    %% - Create 10 MOs then one commit for all MOs.
    %%%%
    Create_10_MO_Time = time_to_create_10_MO_with_attributes(SessionName),
    %%%%
    %% 2. Time to read 100 MO instance, one by one and all together.
    %%%%
    NrOfInstance = ?NrOfInstance,
    %% NrOfInstance = 1,

    ok = create_mo_instance(SessionName, NrOfInstance),

    %%%% 
    %% read one by one, no commit
    %%%
    TimeToReadMOInst_1 = time_to_read_mo_instance_1(SessionName, NrOfInstance),
    %%%% 
    %% read all together at once, no commit
    %%%%
    TimeToReadMOInst_2 = time_to_read_mo_instance_2(SessionName, NrOfInstance),

    %%%%
    %% Delete MO instance, no measure. This is just to clean up before nextstep.
    %%%%    
    ok = delete_mo_instance(SessionName, NrOfInstance),

    %%%%
    %% Background load. 50% load on each CPU.
    %% Use of cpulimit to control load on each CPU.
    %%%%
    ok = generate_cpu_load("50"),
    %% test_server:break("break"),

    %%%% 
    %% 3. Response time using one MO
    %%%
    {TimeToCreate_1_MO, _ , _} = time_to_create_mo_instance(SessionName, 1),
    {TimeToWriteAttribute_1_MO, _ , _} = 
	time_to_write_attribute(SessionName, 1), % add 2 attributes
    {TimeToReadAttribute_1_MO, _ , _} = 
	time_to_read_attributes(SessionName, 1),  % read 2 attributes, no commit
    {TimeToChangeAttribute_1_MO, _ , _} = 
	time_to_change_attribute(SessionName, 1),
    {TimeToDelete_1_MO, _ , _} = 
	time_to_delete_mo(SessionName, 1),

    %%%% 
    %% 4. Average response time when using 100 MO.
    %%%
    {TimeToCreate_100_MO, 
     MinTimeToCreate, 
     SortedTimeToCreate} = 
	time_to_create_mo_instance(SessionName, NrOfInstance),
    MedianTimeToCreate = lists:nth(?Median, SortedTimeToCreate),

    {TimeToWriteAttribute_100_MO, 
     MinTimeToWriteAttribute, 
     SortedTimeToWriteAttribute}  = 
	time_to_write_attribute(SessionName, NrOfInstance),
    MedianTimeToWriteAttribute = lists:nth(?Median, SortedTimeToWriteAttribute),

    {TimeToReadAttribute_100_MO, 
     MinTimeToReadAttribute, 
     SortedTimeToReadAttribute}  = 
	time_to_read_attributes(SessionName, NrOfInstance),
    MedianTimeToReadAttribute = lists:nth(?Median, SortedTimeToReadAttribute),

    {TimeToChangeAttribute_100_MO, 
     MinTimeToChangeAttribute, 
     SortedTimeToChangeAttribute}  = 
	time_to_change_attribute(SessionName, NrOfInstance),
    MedianTimeToChangeAttribute = lists:nth(?Median,SortedTimeToChangeAttribute),

    {TimeToDelete_100_MO, 
     MinTimeToDelete, 
     SortedTimeToDelete}  = 
	time_to_delete_mo(SessionName, NrOfInstance),
    MedianTimeToDelete = lists:nth(?Median, SortedTimeToDelete),

    AvTimeToCreate_100_MO = 
	round(TimeToCreate_100_MO/NrOfInstance),
    AvTimeToWriteAttribute_100_MO = 
	round(TimeToWriteAttribute_100_MO/NrOfInstance),
    AvTimeToReadAttribute_100_MO = 
	round(TimeToReadAttribute_100_MO/NrOfInstance),
    AvTimeToChangeAttribute_100_MO = 
	round(TimeToChangeAttribute_100_MO/NrOfInstance),
    AvTimeToDelete_100_MO = 
	round(TimeToDelete_100_MO/NrOfInstance),

    %%%%
    %% Get sw version, using cli
    %%%%
    CXS_label = rct_tlib:get_sw_version(?CLI_Name),

    %%%%
    %% Update log measurement file
    %%%%
    BoardType = meas_lib:get_board_type(),
    Board_Type = case BoardType of
		     "dus5201" ->
			 case meas_lib:check_if_cxs_is_bpu() of
			     "bpu" ->
				 "bpu";
			     _Other ->
				 BoardType
			 end;
		     _Otheeer ->
			 BoardType
		 end,
    ct:log("Board_Type: ~p", [Board_Type]),
    

    %% BuildLabel = meas_lib:get_build_label(),
    Branch = meas_lib:get_branch(),
    FileName = Branch++"_"++Board_Type++"_measure_response_time_nc_operation_imm_oi",
    rct_tlib:
	writeDataToFile(?RESULTDIR,
			FileName, 
			"~p;~w;~w;~w;~w;~w;~w;~w;~w;~w;"
			"~w;~w;~w;~w;~w;~w;~w;~w;~w;~w;"
			"~w;~w;~w;~w;~w;~w~n", 
			[httpd_util:rfc1123_date(),
			 CXS_label,
			 Create_10_MO_Time,
			 TimeToReadMOInst_1,
			 TimeToReadMOInst_2,
			 TimeToCreate_1_MO,
			 TimeToWriteAttribute_1_MO,
			 TimeToReadAttribute_1_MO,
			 TimeToChangeAttribute_1_MO,
			 TimeToDelete_1_MO,
			 AvTimeToCreate_100_MO, 
			 MinTimeToCreate, 
			 MedianTimeToCreate,
			 AvTimeToWriteAttribute_100_MO, 
			 MinTimeToWriteAttribute, 
			 MedianTimeToWriteAttribute,
			 AvTimeToReadAttribute_100_MO, 
			 MinTimeToReadAttribute, 
			 MedianTimeToReadAttribute,
			 AvTimeToChangeAttribute_100_MO, 
			 MinTimeToChangeAttribute, 
			 MedianTimeToChangeAttribute,
			 AvTimeToDelete_100_MO, 
			 MinTimeToDelete, 
			 MedianTimeToDelete,
			 NodeName
			]),

    ct:pal("All results from executed runs is logged in \n"
	   " file: ~p ,\n"
	   " path: ~p \n"
	   " Result output:\n"
	   " Date,\n"
	   " CXS,\n"
	   " Create_10_MO_Time,\n"
	   " TimeToReadMOInst_one_by_one,\n"
	   " TimeToReadMOInst_all_at_once,\n"
	   " \n"
	   " TimeToCreate_1_MO,\n"
	   " TimeToWriteAttribute_1_MO,\n"
	   " TimeToReadAttribute_1_MO,\n"
	   " TimeToChangeAttribute_1_MO,\n"
	   " TimeToDelete_1_MO,\n"
	   " \n"
	   " AvTimeToCreate_100_MO, Min, Median,\n"
	   " AvTimeToWriteAttribute_100_MO, Min, Median,\n"
	   " AvTimeToReadAttribute_100_MO, Min, Median,\n"
	   " AvTimeToChangeAttribute_100_MO, Min, Median,\n"
	   " AvTimeToDelete_100_MO, Min, Median,\n \n",
	   [FileName, ?RESULTDIR]),

    ct:pal(" Create_10_MO_Time: ~w,~n"
	   " TimeToReadMOInst_one_by_one: ~w,~n"
	   " TimeToReadMOInst_all_at_once: ~w,~n"
	   "~n"
	   " TimeToCreate_1_MO: ~w,~n"
	   " TimeToWriteAttribute_1_MO: ~w,~n"
	   " TimeToReadAttribute_1_MO: ~w,~n"
	   " TimeToChangeAttribute_1_MO: ~w,~n"
	   " TimeToDelete_1_MO: ~w,~n"
	   "~n"
	   " AvTimeToCreate_100_MO: ~w, Min: ~w, Median: ~w,~n"
	   " AvTimeToWriteAttribute_100_MO: ~w, Min: ~w, Median: ~w,~n"
	   " AvTimeToReadAttribute_100_MO: ~w, Min: ~w, Median: ~w,~n"
	   " AvTimeToChangeAttribute_100_MO: ~w, Min: ~w, Median: ~w,~n"
	   " AvTimeToDelete_100_MO: ~w, Min: ~w, Median: ~w,~n", 
    	   [Create_10_MO_Time,
    	    TimeToReadMOInst_1,
    	    TimeToReadMOInst_2,
    	    TimeToCreate_1_MO,
	    TimeToWriteAttribute_1_MO,
	    TimeToReadAttribute_1_MO,
	    TimeToChangeAttribute_1_MO,
	    TimeToDelete_1_MO,
	    AvTimeToCreate_100_MO, 
	    MinTimeToCreate, 
	    MedianTimeToCreate,
	    AvTimeToWriteAttribute_100_MO, 
	    MinTimeToWriteAttribute, 
	    MedianTimeToWriteAttribute,
	    AvTimeToReadAttribute_100_MO, 
	    MinTimeToReadAttribute, 
	    MedianTimeToReadAttribute,
	    AvTimeToChangeAttribute_100_MO, 
	    MinTimeToChangeAttribute, 
	    MedianTimeToChangeAttribute,
	    AvTimeToDelete_100_MO, 
	    MinTimeToDelete, 
	    MedianTimeToDelete
    	   ]),

    %%%%
    %% Remove background load on the CPUs, kill cat pids and cpulimit pids.
    %% Note! pkill cat before pkill cpulimit, otherwise a core dump will be done
    %%%%
    timer:sleep(2000),
    %% [] = rct_rpc:call(rpc, os, cmd, ["pkill cat"], 10000, noprint),
    pkill("/usr/bin/pkill cat"),

    timer:sleep(2000),    %% sleep neede, otherwise a core dump will be done
    %% [] = rct_rpc:call(rpc, os, cmd, ["pkill cpulimit"], 10000, noprint),
    pkill("/usr/bin/pkill cpulimit"),

    [] = rct_rpc:call(rpc, os, cmd, ["pgrep cpulimit"], 10000, noprint),
    [] = rct_rpc:call(rpc, os, cmd, ["pgrep cat"], 10000, noprint),

    %%%%
    %% Remove cpulimit.
    %%%%
    [] = rct_rpc:call(rpc, os, cmd, ["rm "++?CPULIMIT_TO_PATH++"cpulimit"], 
		      10000, noprint),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    ok.


pkill(PkillCmd) ->
    ct:pal("pkill cmd :  ~p.", [PkillCmd]),
    pkill(PkillCmd, 60000).
pkill(_PkillCmd, Timeout) when Timeout < 500 ->
    ct:fail("pkill did not work, within max timeout after restart.");
pkill(PkillCmd, Timeout) ->
    case rct_rpc:call(rpc, os, cmd, [PkillCmd], 10000, print) of
	[] ->
	    ok;
	Res ->
	    ct:pal("pkill did not work :  ~p , sleep 5sec and try again.", [Res]),
	    rct_rpc:call(rpc, os, cmd, ["ls -l /usr/bin/"], 10000, print),
	    rct_rpc:call(rpc, os, cmd, ["ls -l /usr/bin/pkill"], 10000, print),

	    timer:sleep(5000),
	    pkill(PkillCmd, Timeout-5000)
    end.

	

%% ===========================================================================
%% @doc
%% Response time to create 10MO with attributes and then commit. <br/>
%% - Create 10 MO instance, with attribute. <br/>
%% - Commit. <br/>
%% - Cleanup, delete MOs and close session. <br/>
%% @spec time_to_create_10_MO_with_attributes(SessionName) -> TimeToAdd_10_MO
%% @end
%% ===========================================================================
time_to_create_10_MO_with_attributes(SessionName)->
    %%%%
    %% Precondition is a open netconf sessions is up and running.
    %%%%    
    ct:pal("### Open NC session, Name: ~p", [SessionName]),
    {ok,_} = ct_netconfc:open(SessionName,[]),

    %%%%
    %% Create a MO instance.
    %%%%
    NrList = lists:seq(1,10),
    Start1 = os:timestamp(),
    lists:foreach(fun(Nr)->
			  InstName = atom_to_list(SessionName)++"_"++
			      integer_to_list(Nr),
			  AttrName = integer_to_list(Nr),
			  ok = rct_nc_testclass1_lib:
			      nc_add_mo_instance_and_attr(SessionName, 
							  InstName, 
							  AttrName)
		  end, NrList),
    ok = ct_netconfc:close_session(SessionName),
    End1 = os:timestamp(),

    TimeToAdd_10_MO = trunc(timer:now_diff(End1, Start1) / 1000),
    ct:pal("TimeToAdd_10_MO: ~p ms.",[TimeToAdd_10_MO]),

    %%%%
    %% Delete MO instances.
    %%%%
    {ok,_} = ct_netconfc:open(SessionName,[]),
    lists:foreach(
      fun(Nr1)->
	      InstName1 = 
		  atom_to_list(SessionName)++"_"++integer_to_list(Nr1),
	      ok = rct_nc_testclass1_lib:
		  nc_delete_mo_instance(SessionName, 
					InstName1)
      end, NrList),
    
    %%%%
    %% Close netconf session.
    %%%%    
    ok = ct_netconfc:close_session(SessionName),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    TimeToAdd_10_MO.

%% ===========================================================================
%% @doc
%% Response time to read 100 MO, one by one. <br/>
%% - Open session <br/>
%% - read MO one by one. <br/>
%% - Close session.
%% @spec time_to_read_mo_instance_1(SessionName, NrOfInstance) -> TimeToReadMoInst
%% @end
%% ===========================================================================
time_to_read_mo_instance_1(SessionName, NrOfInstance)->
    %%%%
    %% Open netconf session.
    %%%%    
    ct:pal("### Open NC session, Name: ~p", [SessionName]),
    {ok,_} = ct_netconfc:open(SessionName,[]),

    NrList = lists:seq(1,NrOfInstance),
    Start1 = os:timestamp(),
    lists:foreach(fun(Nr)->
			  InstName = atom_to_list(SessionName)++"_"++
			      integer_to_list(Nr),
			  ok = rct_nc_testclass1_lib:
			      nc_get_mo_instance_check(SessionName, 
						       InstName)
			      
		  end, NrList),
    End1 = os:timestamp(),
    TimeToReadMoInst = trunc(timer:now_diff(End1, Start1) / 1000),
    ct:pal("Time to read ~p MO instance one by one: ~p ms.",
	   [NrOfInstance, TimeToReadMoInst]),

    %%%%
    %% Close netconf session.
    %%%%    
    ok = ct_netconfc:close_session(SessionName),
 
    TimeToReadMoInst.

%% ===========================================================================
%% @doc
%% Response time to read 100 MO, read all together at same time. <br/>
%% - Open session <br/>
%% - read all MO at same time. <br/>
%% - Close session.
%% @spec time_to_read_mo_instance_2(SessionName, NrOfInstance) -> TimeToReadMoInst
%% @end
%% ===========================================================================
time_to_read_mo_instance_2(SessionName, NrOfInstance)->
    %%%%
    %% Open netconf session.
    %%%%    
    ct:pal("### Open NC session, Name: ~p", [SessionName]),
    {ok,_} = ct_netconfc:open(SessionName,[]),

    NrOfMoInstList = lists:seq(1,NrOfInstance),

    Start1 = os:timestamp(),
    rct_nc_testclass1_lib:nc_get_several_mo_instance_check(SessionName,
							   NrOfMoInstList),
    End1 = os:timestamp(),
    TimeToReadMoInst = trunc(timer:now_diff(End1, Start1) / 1000),
    ct:pal("Time to read ~p MO instance one by one: ~p ms.",
	   [NrOfInstance, TimeToReadMoInst]),

    %%%%
    %% Close netconf session.
    %%%%    
    ok = ct_netconfc:close_session(SessionName),

    TimeToReadMoInst.

%% ===========================================================================
%% @doc
%% Create MO instances. No measure responce time <br/>
%% - Open session <br/>
%% - Create MO. <br/>
%% - Close session.
%% @spec create_mo_instance(SessionName, NrOfInstance) -> ok
%% @end
%% ===========================================================================
create_mo_instance(SessionName, NrOfInstance)->
    %%%%
    %% Open netconf session.
    %%%%    
    ct:pal("### Open NC session, Name: ~p", [SessionName]),
    {ok,_} = ct_netconfc:open(SessionName,[]),
    
    %%%%
    %% Create a MO instance.
    %%%%
    NrList = lists:seq(1,NrOfInstance),
    lists:foreach(fun(Nr)->
			  InstName = atom_to_list(SessionName)++"_"++
			      integer_to_list(Nr),
			  rct_nc_testclass1_lib:
			      nc_add_mo_instance(SessionName, 
						 InstName)
		  end, NrList),
    
    %%%%
    %% Commit
    %%%%
    ok = ct_netconfc:close_session(SessionName),

    ct:pal("~p MO instances is created. ",[NrOfInstance]),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),
    ok.

%% ===========================================================================
%% @doc
%% Response time to create MO. Open session will not be mesured.<br/>
%% - Open session, Will not be measured <br/>
%% - Create MO. <br/>
%% - Commit. <br/>
%% @spec time_to_create_mo_instance(SessionName, NrOfInstance) -> TotTimeToCreateMoInst
%% @end
%% ===========================================================================
time_to_create_mo_instance(SessionName, NrOfInstance) ->
    NrList = lists:seq(1,NrOfInstance),

    Times = lists:map(fun(Nr)->
			      %%%%
			      %% Open session.
			      %%%%    
			      {ok,_} = ct_netconfc:open(SessionName,[]),
			      InstName = atom_to_list(SessionName)++"_"++
				  integer_to_list(Nr),
			      Start1 = os:timestamp(),
			      %%%%
			      %% Create a MO instance.
                              %%%%
			      rct_nc_testclass1_lib:
				  nc_add_mo_instance(SessionName, 
						     InstName),
			      %%%%
			      %% Commit
			      %%%%
			      ok = ct_netconfc:close_session(SessionName),
			      End1 = os:timestamp(),
			      TimeToCreateMoInst = 
				  trunc(timer:now_diff(End1, Start1) / 1000),
			      TimeToCreateMoInst
		      end, NrList),

    SortedTimes = lists:sort(Times),
    ct:log("CreateMoIns Times: ~n~w~n",[Times]),
    ct:log("Sorted CreateMoIns Times: ~n~w",[SortedTimes]),

    TotTimeToCreateMoInst=lists:foldl(fun(X, Sum) -> 
					      X + Sum 
				      end, 0, Times),

    ct:pal("~p MO instances is created. TimeToCreate: ~p",
	   [NrOfInstance,TotTimeToCreateMoInst]),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),
        {TotTimeToCreateMoInst, lists:min(Times), SortedTimes}.
    %% {TotTimeToCreateMoInst, lists:min(Times), lists:max(Times)}.

%% ===========================================================================
%% @doc
%% Response time to write attribute in MO. <br/>
%% Open session will not be mesured.<br/>
%% - Open session, Will not be measured <br/>
%% - Write attribute in MO. <br/>
%% - Commit. <br/>
%% @spec time_to_write_attribute(SessionName, NrOfInstance) -> TotTimeToWriteAttribute
%% @end
%% ===========================================================================
time_to_write_attribute(SessionName, NrOfInstance) ->

    NrList = lists:seq(1,NrOfInstance),
    Times = lists:map(fun(Nr)->
                              %%%%
			      %% Open session.
			      %%%%    
			      {ok,_} = ct_netconfc:open(SessionName,[]),
			      InstName = atom_to_list(SessionName)++"_"++
				  integer_to_list(Nr),
			      AttrName = integer_to_list(Nr),
			      Start1 = os:timestamp(),
                              %%%%
			      %% Create a MO instance.
                              %%%%
			      ok = rct_nc_testclass1_lib:
				  nc_add_mo_instance_and_attr(SessionName, 
							      InstName, 
							      AttrName),
			      %%%%
			      %% Commit
                              %%%%
			      ok = ct_netconfc:close_session(SessionName),
			      End1 = os:timestamp(),
			      TimeToWriteAttribute = 
				  trunc(timer:now_diff(End1, Start1) / 1000),
			      TimeToWriteAttribute
		  end, NrList),

    SortedTimes = lists:sort(Times),
    ct:log("WriteAttribute Times: ~n~w~n",[Times]),
    ct:log("Sorted WriteAttribute Times: ~n~w",[SortedTimes]),

    TotTimeToWriteAttribute = lists:foldl(fun(X, Sum) -> 
					      X + Sum 
				      end, 0, Times),

    ct:pal("~p Attributes in MO instances is written. TimeToWriteAttribute: ~p",
	   [NrOfInstance,TotTimeToWriteAttribute]),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    {TotTimeToWriteAttribute, lists:min(Times), SortedTimes}.

%% ===========================================================================
%% @doc
%% Response time to read attribute in MO. Open session will not be mesured.<br/>
%% - Open session, Will not be measured <br/>
%% - read attribute in MO. <br/>
%% - Close session, Will not be measured. <br/>
%% @spec time_to_read_attributes(SessionName, NrOfInstance) -> TimeToReadAttribute
%% @end
%% ===========================================================================
time_to_read_attributes(SessionName, NrOfInstance)->
    %%%%
    %% Open netconf session.
    %%%%    
    ct:pal("### Open NC session, Name: ~p", [SessionName]),
    {ok,_} = ct_netconfc:open(SessionName,[]),

    %%%%
    %% Create a MO instance.
    %%%%
    NrList = lists:seq(1,NrOfInstance),
    Times = lists:map(fun(Nr)->
			      InstName = atom_to_list(SessionName)++"_"++
				  integer_to_list(Nr),
			      AttrName = integer_to_list(Nr),
			      Start1 = os:timestamp(),
			      ok = rct_nc_testclass1_lib:
				  nc_get_attribute_check(SessionName, 
							 InstName, 
							 AttrName),
			      End1 = os:timestamp(),
			      TimeToReadAttribute = 
				  trunc(timer:now_diff(End1, Start1) / 1000),
			      TimeToReadAttribute
		      end, NrList),

    SortedTimes = lists:sort(Times),
    ct:log("ReadAttribute Times: ~n~w~n",[Times]),
    ct:log("Sorted ReadAttribute Times: ~n~w",[SortedTimes]),

    TotTimeToReadAttribute= lists:foldl(fun(X, Sum) -> 
					      X + Sum 
				      end, 0, Times),
    %%%%
    %% Commit
    %%%%
    ok = ct_netconfc:close_session(SessionName),
    ct:pal("Nr of MOs: ~p, TimeToReadAttribute: ~p",
	   [NrOfInstance, TotTimeToReadAttribute]),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    {TotTimeToReadAttribute, lists:min(Times), SortedTimes}.

%% ===========================================================================
%% @doc
%% Response time to change attribute in MO. <br/>
%% Open session will not be mesured.<br/>
%% - Open session, Will not be measured <br/>
%% - change attribute in MO. <br/>
%% - commit. <br/>
%% @spec time_to_change_attribute(SessionName, NrOfInstance) -> TotTimeToChangeAttribute
%% @end
%% ===========================================================================
time_to_change_attribute(SessionName, NrOfInstance) ->
    %%%%
    %% Create a MO instance.
    %%%%
    NrList = lists:seq(1,NrOfInstance),
    Times = lists:map(fun(Nr)->
                              %%%%
			      %% Open session.
			      %%%%    
			      {ok,_} = ct_netconfc:open(SessionName,[]),
			      Start1 = os:timestamp(),
			      InstName = atom_to_list(SessionName)++"_"++
				  integer_to_list(Nr),
			      NewAttr = 
				  integer_to_list(Nr)++integer_to_list(Nr),
			      ok = rct_nc_testclass1_lib:
				  nc_add_mo_instance_and_attr(SessionName, 
							      InstName, 
							      NewAttr),
			      %%%%
			      %% Commit
                              %%%%
			      ok = ct_netconfc:close_session(SessionName),
			      End1 = os:timestamp(),
			      TimeToChangeAttribute = 
				  trunc(timer:now_diff(End1, Start1) / 1000),
			      TimeToChangeAttribute
		  end, NrList),

    SortedTimes = lists:sort(Times),
    ct:log("ChangeAttribute Times: ~n~w~n",[Times]),
    ct:log("Sorted ChangeAttribute Times: ~n~w",[SortedTimes]),

    TotTimeToChangeAttribute = lists:foldl(fun(X, Sum) -> 
					      X + Sum 
				      end, 0, Times),

    ct:pal("~p Change Attributes in MO instances. TimeToChangeAttr: ~p",
	   [NrOfInstance,TotTimeToChangeAttribute]),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    {TotTimeToChangeAttribute, lists:min(Times), SortedTimes}.

%% ===========================================================================
%% @doc
%% Response time to delete MO. Open session will not be mesured.<br/>
%% - Open session, Will not be measured <br/>
%% - Delete MO. <br/>
%% - commit. <br/>
%% @spec time_to_delete_mo(SessionName, NrOfInstance) -> TotTimeToDeleteMO
%% @end
%% ===========================================================================
time_to_delete_mo(SessionName, NrOfInstance) ->
    NrList = lists:seq(1,NrOfInstance),
    Times = lists:map(fun(Nr)->
			      %%%%
			      %% Open session
			      %%%%
			      {ok,_} = ct_netconfc:open(SessionName,[]),
			      Start1 = os:timestamp(),
			      InstName = atom_to_list(SessionName)++"_"++
				  integer_to_list(Nr),
			      ok = rct_nc_testclass1_lib:
				  nc_delete_mo_instance(SessionName, 
							InstName),
                              %%%%
			      %% Commit
			      %%%%
			      ok = ct_netconfc:close_session(SessionName),
			      End1 = os:timestamp(),
			      TimeToDeleteMO = 
				  trunc(timer:now_diff(End1, Start1) / 1000),
			      TimeToDeleteMO
		      end, NrList),

    SortedTimes = lists:sort(Times),
    ct:log("DeleteMO Times: ~n~w~n",[Times]),
    ct:log("Sorted DeleteMO Times: ~n~w",[SortedTimes]),

    TotTimeToDeleteMO = lists:foldl(fun(X, Sum) -> 
					      X + Sum 
				      end, 0, Times),

    ct:pal("~p MO instances is deleted. TimeToDelete: ~p",
	   [NrOfInstance,TotTimeToDeleteMO]),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    {TotTimeToDeleteMO, lists:min(Times), SortedTimes}.

%% ===========================================================================
%% @doc
%% Delete MO. this will not be mesured.<br/>
%% - Open session. <br/>
%% - Delete MO. <br/>
%% - commit. <br/>
%% @spec delete_mo_instance(SessionName, NrOfInstance) -> ok
%% @end
%% ===========================================================================
delete_mo_instance(SessionName, NrOfInstance)->
    %%%%
    %% open netconf sessions.
    %%%%    
    ct:pal("### Open NC session, Name: ~p", [SessionName]),
    {ok,_} = ct_netconfc:open(SessionName,[]),
    
    %%%%
    %% Delete a MO instance.
    %%%%
    NrList = lists:seq(1,NrOfInstance),
    lists:foreach(fun(Nr)->
			  InstName = atom_to_list(SessionName)++"_"++
			      integer_to_list(Nr),
			  ok = rct_nc_testclass1_lib:
			      nc_delete_mo_instance(SessionName, 
						    InstName)
		  end, NrList),

    %%%%
    %% Commit
    %%%%
    ct:pal("### Close NC session, Name: ~p", [SessionName]),
    ok = ct_netconfc:close_session(SessionName),

    ct:pal("~p MO instances is deleted.",[NrOfInstance]),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),
    ok.


%% Used by cleanup if tc fail.
%% ===========================================================================
%% @doc
%% Delete MO no check. this will not be mesured. <br/>
%% Used in cleanup if TC fail<br/>
%% - Delete MO. <br/>
%% @spec delete_mo_no_check(SessionName, NrOfInstance) -> ok
%% @end
%% ===========================================================================
delete_mo_no_check(SessionName, NrList)->
    lists:foreach(fun(Nr)->
			  ct_netconfc:open(SessionName,[]),
			  InstName = atom_to_list(SessionName)++"_"++
			      integer_to_list(Nr),
			  rct_nc_testclass1_lib:
			      nc_delete_mo_instance_no_check(SessionName, 
							     InstName),
			  ct_netconfc:close_session(SessionName)
		  end, NrList).


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
    lists:foreach(fun(_C)->
    			  [] = rct_rpc:call(rpc, os, cmd, 
					    [?CPULIMIT_TO_PATH++
						 "cpulimit -z -i -l "++
						 LoadProcent++
						 " cat /dev/zero > /dev/null &"]
					   , 10000, noprint)
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

    %% Check CPU load.
    {ok, CpuLoad} = rct_tlib:cpuload(load,100),
    lists:foreach(fun({_, Load})->
    			  case Load > 10 of
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


%%%======================
meas_100_create(_Config) ->
    SessionName = ?NC_Name,

    %% ct:pal("### Open NC session, Name: ~p", [SessionName]),
    %% {ok,_} = ct_netconfc:open(SessionName,[]),

    %%%%
    %% Create a MO instance.
    %%%%
    NrList = lists:seq(1,100),
    ct:pal("Start create 100 MO with attr.",[]),

    Start1 = os:timestamp(),
    lists:foreach(fun(Nr)->
			  {ok,_} = ct_netconfc:open(SessionName,[]),
			  InstName = atom_to_list(SessionName)++"_"++
			      integer_to_list(Nr),
			  AttrName = integer_to_list(Nr),
			  ok = rct_nc_testclass1_lib:
			      nc_add_mo_instance_and_attr(SessionName, 
							  InstName, 
							  AttrName),
			  ok = ct_netconfc:close_session(SessionName)
		  end, NrList),
    End1 = os:timestamp(),

    TimeToAdd_100_MO = trunc(timer:now_diff(End1, Start1) / 1000),
    ct:pal("TimeToAdd_100_MO: ~p ms.",[TimeToAdd_100_MO]),

    CXS_label = rct_tlib:get_sw_version(?CLI_Name),

    Nr = integer_to_list(length(NrList)),
    FileName = "create_"++Nr++"_MOs.txt",
    [{_, NodeName}] = ct:get_config(test_nodes),
        ct:pal("NodeName: ~p",[NodeName]),

    MeasInfo = [httpd_util:rfc1123_date(), 
		CXS_label, 
		TimeToAdd_100_MO, 
		NodeName],
    ct:pal("MeasInfo: ~p",[MeasInfo]),

    rct_tlib:writeDataToFile(?RESULTDIR_Test, 
			FileName, 
			"~p;~w;~w;~w~n", 
			MeasInfo),

    ct:pal("results file: ~p ,\n path: ~p \n",[FileName, ?RESULTDIR_Test]),

    timer:sleep(20000),

    ct:pal("Start delete 100 MO.",[]),
    %%%%
    %% Delete MO instances.
    %%%%
    {ok,_} = ct_netconfc:open(SessionName,[]),
    lists:foreach(
      fun(Nr1)->
	      InstName1 = 
		  atom_to_list(SessionName)++"_"++integer_to_list(Nr1),
	      ok = rct_nc_testclass1_lib:
		  nc_delete_mo_instance(SessionName, 
					InstName1)
      end, NrList),
    
    %%%%
    %% Close netconf session.
    %%%%    
    ok = ct_netconfc:close_session(SessionName),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    TimeToAdd_100_MO,

    timer:sleep(30000).

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

%%% #0.    BASIC INFORMATION
%% coding: latin-1
%%% ----------------------------------------------------------
%%% %CCaseFile:	xxl_rob_netconf_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R5A/4
%%% 
%%% @doc == Measure memory usages when netconf users operates.==
%%% This Test Suite can be used on target enviroment.
%%% Used sw version and measured data is written to a file.
%%% Path: /proj/rcs/measurements/
%%%
%%% <br/><br/>
%%% 
%%% @end

-module(xxl_rob_netconf_SUITE).
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
%%% R5A/2      2015-12-14 etxivri     Created
%%% R5A/3      2016-01-21 erarafo     Fixed compiler warning and copyright
%%% R5A/4      2016-02-19 etxkols     Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
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
	 nc_operation_150_hour_no_check/1
	]).

-define(NrOfNetConfUsers, 5).
%% -define(NrOfNetConfUsers, 20).
-define(NC_SessionNameList, 
	[list_to_atom(
	   "nc"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfNetConfUsers)]).


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
    %% CliHooks = 
    %% 	[{rct_cli, 
    %% 	  {list_to_atom(
    %% 	     "cli"++integer_to_list(N)), 
    %% 	   [manual_connect]}} || N <- lists:seq(1,?NrOfCliUsers)],

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
		 %% {rct_cli, {cli1, [manual_connect]}},
		 %% {rct_tlib,{identifier,[]}},
		 {rct_scp, [{1, node1}]},
                 %% %% {rct_core,[]} | NetconfHooks
		 {rct_core,[]} | NetconfHooks
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
    A = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 
			     10000, noprint),
    C = rct_rpc:call(rpc, os, cmd, ["pgrep netconf"], 10000, noprint),
    D = rct_rpc:call(rpc, os, getpid, [], 10000),
    
    ct:pal("### Com: ~p",[A]),
    ct:pal("### Netconf: ~p",[C]),
    ct:pal("### BeamPid: ~p .",[D]),
    Config.


%% @hidden
end_per_testcase(_TestCase, Config) ->
    ct:pal("End per TC.", []),

    A = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 
		     10000, noprint),
    C = rct_rpc:call(rpc, os, cmd, ["pgrep netconf"], 10000, noprint),
    D = rct_rpc:call(rpc, os, getpid, [], 10000),

    ct:pal("### Com: ~p",[A]),
    ct:pal("### Netconf: ~p",[C]),
    ct:pal("### BeamPid: ~p .",[D]),

    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p.  \nClean up added instances", 
		   [Reason]),

	    %% A = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 
	    %% 		     5000, noprint),
	    %% B = rct_rpc:call(rpc, os, cmd, ["pgrep cli"], 5000, noprint),
	    %% C = rct_rpc:call(rpc, os, cmd, ["pgrep netconf"], 5000, noprint),
	    %% D = rct_rpc:call(rpc, os, getpid, [], 1000),

	    %% ct:pal("### Com: ~p",[A]),
	    %% ct:pal("### Cli: ~p",[B]),
	    %% ct:pal("### Netconf: ~p",[C]),
	    %% ct:pal("### BeamPid: ~p .",[D]),


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
	    		  ?NC_SessionNameList)
    end,

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
nc_operation_1_hour(_Config) ->
    %% reboot_node(),
    do_nc_operation(1, no_check, nc).

nc_operation_10_hour_no_check(_Config) ->
    %% reboot_node(),
    do_nc_operation(10, no_check, nc).

nc_operation_150_hour_no_check(_Config) ->
    %% reboot_node(),
    do_nc_operation(150, no_check, nc).


%% ===========================================================================
%% @doc
%% This will do netconf operations and measure used memory size. <br/>
%% Fragment shall be owened by an application <br/>
%% The measured data will be stored in file. <br/>
%% Hours = integer() <br/>
%% LogPath = Path to ct logs directory. <br/>
%% CheckFlag = check | no_check <br/>
%% OperType = nc | cli | {nc, cli} <br/>
%% @spec do_nc_operation(Hours, CheckFlag, OperType) -> ok
%% @end
%% ===========================================================================
do_nc_operation(Hours, CheckFlag, OperType) ->
    %%%%
    %% Get sw version, using cli
    %%%%
    %% CXS_label = get_sw_version(),

    Seconds = Hours*60*60,

    %% [{_, NodeName}] = ct:get_config(test_nodes),

   
    StartTime = os:timestamp(),
    
    Nr = loop_operations(StartTime, 
			 Seconds,
			 %% 120,
			 CheckFlag,
			 OperType),
    ct:pal("End Nr: ~p", [Nr]),

    StopTime = os:timestamp(),
    TimeNC_Operation = 
    	trunc(timer:now_diff(StopTime, StartTime) / 1000 / 1000),
    ct:pal("test time: ~p", [TimeNC_Operation]),

    %%%%
    %% check nc sessions deleted
    %%%%
    ct:pal("Check nc session is deleted.",[]),
    [] = rct_rpc:call(rpc, os, cmd, ["pgrep netconf"], 10000, noprint),

    ok.


%% ===========================================================================
%% @doc
%% loop. <br/>
%% @end
%% ===========================================================================
loop_operations(StartTime, Duration, CheckFlag, OperType) ->
    loop_operations(StartTime, Duration, 0, CheckFlag, OperType).

loop_operations(StartTime, Duration, Nr, CheckFlag, OperType) ->
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
	_Other ->
	    ct:fail("OperType does not exist")
    end,

    case Nr rem 50 of
	0 ->
	    ct:log("Nr: ~p", [Nr]);
	_ ->
	    ok
    end,

    CheckTime = os:timestamp(),
    TimeDiff = 
	trunc(timer:now_diff(CheckTime, StartTime) / 1000 / 1000),
    %% ct:pal("TimeDiff: ~p", [TimeDiff]),

    case TimeDiff > Duration of
	true ->
	    ct:log("Nr: ~p", [Nr]),
	    Nr;
	false ->
	    loop_operations(StartTime, Duration, Nr+1, CheckFlag, OperType)
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
%%     net_kernel:disconnect(ErlNode),
%%     ok = ct_telnet:send(console, "reboot"),
%%     timer:sleep(5000),
%%     {ok,_} = ct_telnet:expect(console, "login:", [{timeout,60000}, 
%% 						     no_prompt_check]),
%%     net_kernel:connect(ErlNode),

%%     %%%%
%%     %% Wait for testnode up.
%%     %%%%

%%     %%%%
%%     %% Sleep to be sure that com is up and ready to be used.
%%     %%%%
%%     timer:sleep(35000).

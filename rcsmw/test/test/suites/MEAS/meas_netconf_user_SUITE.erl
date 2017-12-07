%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	meas_netconf_user_SUITE.erl %
%%% @author erarafo
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R5A/1
%%% 
%%% @doc == Measure times when one nc user add, get, delete several mo instances.==
%%% This Test Suite can be used on target and simulated enviroment.<br/>
%%%
%%% 
%%% @end

-module(meas_netconf_user_SUITE).
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
%%% R2A/1      2013-06-26 etxivri     Created
%%% R5A/1      2016-01-21 erarafo     Fixed deprecation warning and copyright
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
	 add_get_delete_several_rbsUnits/1,
	 add_get_delete_several_rbsUnits_one_commit/1
	]).

-define(NrOfNetConfUsers, 1).
%% -define(NrOfNetConfUsers, 50).

-define(NrOfMoInstList, lists:seq(1,200) ).

-define(NC_SessionNameList, [list_to_atom("nc"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfNetConfUsers)]).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% runs the rct_netconf hook for each user, to be able to open a netconf session for each user.<br/>
%% @end
%%--------------------------------------------------------------------
suite() -> 
    %% build up a list of NetconfHooks touples with differents Names.
    NetconfHooks = [{rct_netconf, list_to_atom("nc"++integer_to_list(N))} || N <- lists:seq(1,?NrOfNetConfUsers)],
    %% ct:pal("NetconfHooks: ~p",[NetconfHooks]),
    %% [{timetrap, {minutes, 600}}, % 10 hours
    [{timetrap, {hours, 72}}, % 10 hours
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {cth_conn_log, []},
		 {rct_power,node},
		 {rct_consserv,cs1},
                 {rct_rs232,console},
		 {rct_core,[]},
		 %% {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}} | 
		 {rct_logging, {all, [{erlang,{[],[]}}]}} |
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
    	    ct:pal("Testcase failed due to: ~p.  \nClean up added rbs units", [Reason]),

	    A = rct_rpc:call(rpc, os, cmd, ["pgrep com -u $USER"], 5000, noprint),
	    C = rct_rpc:call(rpc, os, cmd, ["pgrep netconf -u $USER"], 5000, noprint),
	    CC = length(string:tokens(C,"\n")),
	    ct:pal("### Com: ~p",[A]),
	    ct:pal("### Netconf: ~p",[CC]),	    

	    %%%%
	    %% Open a session and clean up rbsUnits, close all session.
            %%%%
	    try
		lists:foreach(fun(Name) -> nc_open(ct_netconfc:open(Name, []), Name) end, ?NC_SessionNameList)
	    catch throw:{?MODULE, found} ->
		    ok
	    end,

            %%%%
	    %% Commit, close sessions.
            %%%%
	    lists:foreach(fun(Name1) ->
				  %% ct:pal("### Cleanup after fail TC, Close session: ~p", [Name1]),
				  ct_netconfc:close_session(Name1)
			  end, 
			  ?NC_SessionNameList)
	    		 
    end,

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    ok.

nc_open({error,{connection_exists,_}}, Name) ->
    ct_netconfc:close_session(Name), % Clean up at our client, if netconf process was killed on node, 
    ct_netconfc:open(Name, []), % Then set up again.
    nc_delete_rbsunits(Name),
    throw({?MODULE, found});
nc_open({ok,_}, Name) ->
    nc_delete_rbsunits(Name),
    throw({?MODULE, found});
nc_open(_, _) ->
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
    [].


%%--------------------------------------------------------------------
%% @doc
%% Times for one Netconf users, add, get, delete 200 rbs units. <br/>
%% - Open session, add rbs units, commit after each add.<br/>
%% - Read all MOs.<br/>
%% - Open session, delete all rbs units, commit after each delete.<br/>
%% - print the operation times<br/>
%% @spec add_get_delete_several_rbsUnits(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
add_get_delete_several_rbsUnits(_Config) ->
    ct:pal("### Times for one Netconf users, add, get, delete  200 rbsUnits.!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    [NC_session|_T]=?NC_SessionNameList,

    %%%%
    %% Add rbs units
    %%%%
    ct:pal("Add 200 MO instances.",[ ]),
    Start1 = erlang:timestamp(),

    lists:foreach(fun(Nr) ->  
			  
                          %%%%
			  %% Open Netconf sessions.
                          %%%%
			  ok = nc_open_session([NC_session]),

    			  RbsUnitId = "nc" ++ integer_to_list(Nr),
    			  UserLabel = "userLabel_" ++ RbsUnitId,
    			  %% ct:pal("### Create RBS Unit id: ~p, userlabel: ~p", [RbsUnitId, UserLabel]),
			  
    			  ok = ct_netconfc:edit_config(NC_session,running,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
									   [{managedElementId,[],["1"]},
									    {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
									     [{'equipmentId',[],["1"]},
									      {'RbsUnit',[{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
									       [{rbsUnitId,[],[RbsUnitId]},{userLabel,[],[UserLabel]} ]}]}]}),
			  %%%%
			  %% Close session
                          %%%%
			  ok = nc_close_session([NC_session]) 
    		  end, 
    		  ?NrOfMoInstList),
    
    End1 = erlang:timestamp(),
    
    TimeToAdd = trunc(timer:now_diff(End1, Start1) / 1000),
    ct:pal("TimeToAdd 200 rbsUnits: ~p ms.",[TimeToAdd]),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 5000, noprint),
    F = rct_rpc:call(rpc, ets, tab2list, [rbsUnit], 5000, noprint),

    case length(F) of
	200 ->
	    ok;
	_ ->
	    ct:pal("~p",[length(F)]),
	    ct:fail("Some RbsUnits not created !!")
    end,

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session([NC_session]),

    ct:pal("Read all MOs.",[]),

    Start2 = erlang:timestamp(),
    {ok,_} = ct_netconfc:get(NC_session,{'ManagedElement',
					 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
					 [{managedElementId,[],["1"]}]}),

    End2 = erlang:timestamp(),
    TimeToGet = trunc(timer:now_diff(End2, Start2) / 1000),
    ct:pal("TimeToGet 200 rbsUnits: ~p ms.",[TimeToGet]),
    ok = nc_close_session([NC_session]),

    %%%%
    %% Delete rbs units
    %%%%
    ct:pal("Delete 200 MO instances.",[]),
    Start3 = erlang:timestamp(),
    lists:foreach(fun(Nr1) ->
			  ok = nc_open_session([NC_session]),

			  RbsUnitId = "nc" ++ integer_to_list(Nr1),
    			  UserLabel = "userLabel_" ++ RbsUnitId,
    			  %% ct:pal("### Delete RBS Unit id: ~p, userlabel: ~p", [RbsUnitId, UserLabel]),

    			  ok = ct_netconfc:edit_config(NC_session,running,{'ManagedElement',
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
			  
			  %%%%
			  %% Close session
                          %%%%
			  ok = nc_close_session([NC_session])

    		  end, 
    		  ?NrOfMoInstList),

    End3 = erlang:timestamp(),
    TimeToDel = trunc(timer:now_diff(End3, Start3) / 1000),
    ct:pal("TimeToDelete 200 rbsUnits then commit: ~p ms.",[TimeToDel]),


    ct:pal("TimeToAdd: ~p, TimeToGet: ~p, TimeToDel: ~p.",[TimeToAdd, TimeToGet, TimeToDel]),

    G = rct_rpc:call(rpc, ets, tab2list, [rbsUnit], 5000, noprint),
    case length(G) of
	0 ->
	    ok;
	_ ->
	    ct:pal("~p",[length(G)]),
	    ct:fail("some RbsUnits still exist after delete!!")
    end,

    ok.


%%--------------------------------------------------------------------
%% @doc
%% Times for one Netconf users, add, get, delete 200 rbs units. Do commit after each operation is done. <br/>
%% - Add rbs units, No commit.<br/>
%% - Commit.<br/>
%% - Read all MOs.<br/>
%% - Delete all rbs units, No commit<br/>
%% - Commit.<br/>
%% - print the operation times<br/>
%% @spec add_get_delete_several_rbsUnits_one_commit(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
add_get_delete_several_rbsUnits_one_commit(_Config) ->
    ct:pal("### One Netconf users, add, get, delete  200 rbsUnits. One commit after each operation is done !",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    [NC_session|_T]=?NC_SessionNameList,
    ok = nc_open_session([NC_session]),

    %%%%
    %% Add rbs units
    %%%%
    RbsUnits_Instances = lists:map(fun(Nr) ->
					   RbsUnitId = "nc" ++ integer_to_list(Nr),
					   UserLabel = "userLabel_" ++ RbsUnitId,
					   {'RbsUnit',[{xmlns,"urn:com:ericsson:ecim:rbsunit"}],[{'rbsUnitId',[],[RbsUnitId]}, {userLabel,[],[UserLabel]} ] }
					       
				   end, 
				   ?NrOfMoInstList),
    %% ct:pal("~p", [RbsUnits_Instances]),
    
    All_rbsUnits_List = lists:append([{'equipmentId',[],["1"]}], RbsUnits_Instances),
    %% ct:pal("~p", [All_rbsUnits_List]),

    ct:pal("Add 200 MO instances, commit after all is added.",[]),
    Start1 = erlang:timestamp(),
    ok = ct_netconfc:edit_config(NC_session,running,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    						     [{managedElementId,[],["1"]},
    						      {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],    						       
    						       All_rbsUnits_List
    						      }]}),

    %%%%
    %% Close session
    %%%%
    ok = nc_close_session([NC_session]),
    End1 = erlang:timestamp(),

    TimeToAdd = trunc(timer:now_diff(End1, Start1) / 1000),
    ct:pal("TimeToAdd 200 rbsUnits then commit: ~p ms.",[TimeToAdd]),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 5000, noprint),
    F = rct_rpc:call(rpc, ets, tab2list, [rbsUnit], 5000, noprint),

    case length(F) of
	200 ->
	    ok;
	_ ->
	    ct:pal("~p",[length(F)]),
	    ct:fail("Some RbsUnits not created !!")
    end,

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session([NC_session]),

    ct:pal("Read all MOs.",[]),

    Start2 = erlang:timestamp(),
    {ok,_} = ct_netconfc:get(NC_session,{'ManagedElement',
    						   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    						   [{managedElementId,[],["1"]}]}),

    End2 = erlang:timestamp(),
    TimeToGet = trunc(timer:now_diff(End2, Start2) / 1000),
    ct:pal("TimeToGet 200 rbsUnits then commit: ~p ms.",[TimeToGet]),


    %%%%
    %% Delete rbs units
    %%%%
    RbsUnitsInstances = lists:map(fun(Nr1) ->
					  RbsUnitId = "nc" ++ integer_to_list(Nr1),
					  UserLabel = "userLabel_" ++ RbsUnitId,
					  {'RbsUnit',[{xmlns,"urn:com:ericsson:ecim:rbsunit"},
						      {'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"}, 
						      {'nc:operation',"delete"}],
					   [{'rbsUnitId',[],[RbsUnitId]}, {userLabel,[],[UserLabel]} ] }
					      
				  end, 
				  ?NrOfMoInstList),
    %% ct:pal("~p", [RbsUnitsInstances]),
    
    AllrbsUnitsList = lists:append([{'equipmentId',[],["1"]}], RbsUnitsInstances),
    %% ct:pal("~p", [AllrbsUnitsList]),

    ct:pal("Delete 200 MO instances, commit after all is deleted.",[]),

    Start3 = erlang:timestamp(),
    ok = ct_netconfc:edit_config(NC_session,running,{'ManagedElement',
    						 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    						     [{managedElementId,[],["1"]},
    						      {'Equipment',
    						       [{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
    						       AllrbsUnitsList
    						      }]}),
    


    %%%%
    %% Close session
    %%%%
    ok = nc_close_session([NC_session]),
    End3 = erlang:timestamp(),
    TimeToDel = trunc(timer:now_diff(End3, Start3) / 1000),
    ct:pal("TimeToDelete 200 rbsUnits then commit: ~p ms.",[TimeToDel]),


    ct:pal("TimeToAdd: ~p, TimeToGet: ~p, TimeToDel: ~p.",[TimeToAdd, TimeToGet, TimeToDel]),

    G = rct_rpc:call(rpc, ets, tab2list, [rbsUnit], 5000, noprint),
    case length(G) of
	0 ->
	    ok;
	_ ->
	    ct:pal("~p",[length(G)]),
	    ct:fail("some RbsUnits still exist after delete!!")
    end,

    ok.


%%% Internal functions
%%%%%%%%%%%%%%%%%
%% Use in cleanup
%% Need to open/close after each delete.
%%%%%%%%%%%%%%%%%
nc_delete_rbsunits(NC_hookName) ->
    lists:foreach(fun(Nr) ->
			  RbsUnitId = "nc" ++ integer_to_list(Nr),
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
		  ?NrOfMoInstList).


%%--------------------------------------------------------------------
%% @doc 
%% Open netconf sessions. <br/>
%% @end
%%--------------------------------------------------------------------
nc_open_session(NC_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  %% ct:pal("### Open Name: ~p", [Name]),
    			  {ok,_} = ct_netconfc:open(Name, [])
    		  end, 
		  NC_SessionNameList),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% Close netconf sessions. <br/>
%% @end
%%--------------------------------------------------------------------
nc_close_session(NC_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  %% ct:pal("### CloseName: ~p", [Name]),
			  ok = ct_netconfc:close_session(Name)
    		  end, 
    		  NC_SessionNameList),
    ok.

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	meas_load_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2015
%%% @version /main/R3A/R4A/2
%%% 
%%% @doc == Load when notification is used.==
%%% This Test Suite can be used on target and simulated enviroment.<br/>
%%%
%%% 
%%% @end

-module(meas_load_SUITE).
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
%%% R3A/1      2015-04-22 etxivri     Created
%%% R4A/1      2015-04-28 etxivri     Update notification check.
%%% ----------------------------------------------------------
%%

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
	 notification_at_create/1,
	 delete/1
	]).

-define(NrOfNetConfUsers, 5).

-define(NC_SessionNameList, [list_to_atom("nc"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfNetConfUsers)]).

-define(NR_OF_MO, 500).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% runs the rct_netconf hook for each user, to be able to open a netconf session for each user.<br/>
%% @end
%%--------------------------------------------------------------------
suite() -> 
    %% build up a list of NetconfHooks touples with differents Names.
    NetconfHooks = [{rct_netconf, list_to_atom("nc"++integer_to_list(N))} || N <- lists:seq(1,?NrOfNetConfUsers)],

    [{timetrap, {hours, 1}}, % 10 hours
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {cth_conn_log, []},
		 {rct_power,node},
		 {rct_consserv,cs1},
                 {rct_rs232,console},
		 {rct_core,[]},
		 {rct_logging, 
		  {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}} |
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
    	    ct:pal("Testcase failed due to: ~p.  \nClean up.", [Reason])
    end,
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases defined in list.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [
    
    ].

%%--------------------------------------------------------------------
%% @doc
%% TCs runed by jenkins.
%% @end
%%--------------------------------------------------------------------
groups() ->
    [
     {group_com_1,[],[
		      notification_at_create
		      %% com_notification_at_delete,
		      %% com_notification_at_change
		     ]}
    ].


%%--------------------------------------------------------------------
%% @doc
%% Verify that the sessions that are subscribers on event notifications <br/>
%% for objectCreated on TestClass1.* will get the expected event after the create <br/>
%% request from both netconf and cli. <br/>
%% @spec notification_at_create(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
notification_at_create(_Config) ->
    %%
    %% Create netconf and cli sessions.
    %%
    ok = nc_open_session(?NC_SessionNameList),
    CreateRequest = ["objectCreated"],

    SessionForCreate = lists:nth(1,?NC_SessionNameList),
    SessionsForSub = ?NC_SessionNameList -- [SessionForCreate],

    lists:foreach(fun(Session) ->
			  ok = nc_add_sub(Session, 
					  [{["ManagedElement=1,TestRoot=1,TestClass1.*"], 
					    CreateRequest}],[])
		  end, SessionsForSub),
    
    Nr = ?NR_OF_MO,
    %% Nr = 50,

    %%
    %%Test that the notification will show if testclass1 is created via netconf 
    %%
    ok = nc_add_testclass1(SessionForCreate, Nr),

    ct:pal("### Close session: ~p", [SessionForCreate]),
    ok = ct_netconfc:close_session(SessionForCreate),
    ct:pal("### Done Close session: ~p", [SessionForCreate]),

    %% Start = now(),

    %% NrCheck = 450,
    %% check_notifications(NrCheck, SessionForCreate, SessionsForSub),
    

    %% %%
    %% %% Simple check on notification from add request.
    %% %% Check last create is notified on sessions that subscribe.
    %% %%
    %% %% InstName = 
    %% %% 	atom_to_list(SessionForCreate)++
    %% %% 	"_"++integer_to_list(Nr),
    %% %% AttrName = integer_to_list(Nr),
    %% InstName = 
    %% 	atom_to_list(SessionForCreate)++
    %% 	"_"++integer_to_list(450),
    %% AttrName = integer_to_list(450),
    %% CheckSess = lists:last(SessionsForSub),
    %% receive
    %% 	{notification,
    %% 	 [{xmlns,_}],
    %% 	 [{eventTime,[],_},
    %% 	  {events,[{xmlns,_},{dnPrefix,[]}],
    %% 	   [{objectCreated,
    %% 	     [{dn,"ManagedElement=1,TestRoot=1,TestClass1="++InstName}],
    %% 	     [{attr,
    %% 	       [{name,"int32"}],
    %% 	       [{v,[],[AttrName]}]} | _]
    %% 	    }]
    %% 	  }] } ->
    %% 	    ct:pal("Netconf: Received expected objectCreated notification. "
    %% 		   "NC sess: ~p, MO inst: ~p", [CheckSess, InstName])
    %% 	%% Other ->
    %% 	%%     ct:pal("Other: ~p", [Other])
    %% after 120000 -> 
    %% 	    ct:pal("Error no notification within time! : ~p, ~p", 
    %% 	           [CheckSess, InstName]),
    %% 	    PP = erlang:process_info(self(), messages),
    %% 	    ct:pal("PP: ~p", [PP]),
    %% 	    ct:fail("Error no notification within time!", [])
    %% 	    %% ct:pal("Error no notification within time! : ~p, ~p", [ChecSess, InstName])
    %% end,

    %% End = now(),
    %% Time = trunc(timer:now_diff(End, Start) / 1000),
    %% ct:pal("Time : ~p ms.",[Time]),

    vmstat(),
    %% NrCheck = 450,
    NrCheck = ?NR_OF_MO,
    check_notifications(NrCheck, SessionForCreate, SessionsForSub),

    %% Check leftovers notifications.
    {messages,[]}  = erlang:process_info(self(), messages),
    %% ct:log("PPPP: ~p", [PPPP]),

    %% vmstat(),
    %% test_server:break("A"),

    %% Delete MOs
    ct:pal("### Open session: ~p", [SessionForCreate]),
    {ok,_} = ct_netconfc:open(SessionForCreate,[]),

    ok = nc_delete_testclass1(SessionForCreate, Nr),

    ok = nc_close_session(?NC_SessionNameList),

    ok.

vmstat() ->
    %% The following example displays a systems virtual memory statistics 
    %% 5 times at 1 second intervals.
    NrOfIntervals = 5, %% Takes about 4-5 seconds
    %% CalcNr = 9, %% First sample is removed.
    VmStat = rct_rpc:call(rpc, os, cmd, 
			  ["vmstat 1 "++integer_to_list(NrOfIntervals)], 
			  30000, noprint),
    ct:pal("VmStat: ~n~s~n",[VmStat]),
    ok.

    %% Vm_Stat = string:tokens(VmStat, " \n-"),
    %% %% ct:pal("Vm_Stat: ~n~p",[Vm_Stat]),

    %% ["wa" | Search_List] = lists:dropwhile(fun(X) ->
    %% 						   X =/= "wa"
    %% 					   end, Vm_Stat),
    %% ct:log("Search_List:~n~p~n",[Search_List]),
    %% %% Remove first sample. due to it could be corrupt!
    %% {_L, SearchList} = lists:split(16, Search_List),
    %% ct:log("New SearchList:~n~p~n",[SearchList]),
    %% NrList = lists:seq(1, CalcNr),
    %% MapList = lists:map(fun(N) ->
    %% 		      Id = list_to_integer(lists:nth((16*N)-1, SearchList)),
    %% 		      Wa = list_to_integer(lists:nth(16*N, SearchList)),
    %% 		      %% ct:pal("Id: ~p, Wa: ~p",[Id, Wa]),
    %% 		      {Id, Wa}
    %% 	      end, NrList),
    %% ct:pal("MapList:~n~p~n",[MapList]),

    %% {C,D} = lists:unzip(MapList),
    %% SumId = lists:sum(C),
    %% SumWa = lists:sum(D),
    %% ct:pal("Sum Id: ~p, Sum Wa: ~p",[SumId, SumWa]),
    %% AvgId = SumId/CalcNr,
    %% AvgWa = SumWa/CalcNr,
    %% ct:pal("Avg Id: ~p, Avg Wa: ~p, on ~p intervals", [AvgId, 
    %% 						       AvgWa, 
    %% 						       CalcNr]),
    %% {AvgId, AvgWa}.


delete(_Config) ->
    Session = lists:nth(1,?NC_SessionNameList),
    Nr = ?NR_OF_MO,
    {ok,_} = ct_netconfc:open(Session,[]),
    ok = nc_delete_testclass1(Session, Nr),
    ok = ct_netconfc:close_session(Session),
    ok.


%%% Internal functions
%%--------------------------------------------------------------------
%% @doc 
%% Test COM AVC by creating a subsciption for event.<br/>
%% @end
%%--------------------------------------------------------------------
nc_add_sub(NC_hookName, [], Filter) ->
    ct:pal("### Add subsciption filter: ~p for ~p", [Filter,NC_hookName]),
    %%
    %% Create subscribtion initiates an event notification subscription that sends asynchronous 
    %% event notifications to the initiator of the command in the NETCONF session until the 
    %% subscription terminates.
    %%
    ct_netconfc:create_subscription(NC_hookName, Filter);
nc_add_sub(NC_hookName, [{Obj, FilterType}| Rest], Filter) ->
    NewFilter = create_filter(FilterType, Obj),
    nc_add_sub(NC_hookName, Rest, Filter ++ NewFilter).
    
create_filter(FilterType, RegExp) ->
    %% A list should work with workaround from Siri
    %% [
    {'event', [],
     [{'filterType', [], FilterType},
      {'filterValue', [], RegExp}]}
     %% ]
	.

%%--------------------------------------------------------------------
%% @doc 
%% Test COM AVC by creating a TestClass1 in FAKE.<br/>
%% @end
%%--------------------------------------------------------------------
nc_add_testclass1(NC_hookName, Nr) ->
    ct:pal("Create nr of MO with attribute. Nr : ~p", [Nr]),
    NrList = lists:seq(1, Nr),

    lists:foreach(fun(X)->
			  InstName = 
			      atom_to_list(NC_hookName)++
			      "_"++integer_to_list(X),
			  AttrName = integer_to_list(X),
			  ok = rct_nc_testclass1_lib:
			      nc_add_mo_instance_and_attr(NC_hookName, 
			  				  InstName, 
			  				  AttrName)
		  end, NrList),
    
    ct:pal("Create Done.", []).


%% nc_add_testclass1(NC_hookName) ->
%%     TestClass1_Id = atom_to_list(NC_hookName),
%%     Int32 = ?NC_INT32,
%%     ct:pal("### Create TestClass1: ~p, Int32: ~p", [TestClass1_Id, Int32]),

%%     ok = ct_netconfc:edit_config(NC_hookName,running,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
%% 						      [{managedElementId,[],["1"]},
%% 						       {'TestRoot',
%% 							[{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
%% 							[{testRootId,[],["1"]},
%% 							 {'TestClass1',
%% 							  [],
%% 							  [{'testClass1Id',[],[TestClass1_Id]},
%% 							   {int32,[],[Int32]}
%% 							  ]}]}]}, 30000),
%%     ok.


%%--------------------------------------------------------------------
%% @doc 
%% Test netconf delete by deleteing a TestClass1 in FAKE.<br/><br/>
%% @end
%%--------------------------------------------------------------------
nc_delete_testclass1(NC_hookName, Nr) ->
    ct:pal("Delete nr of MO. Nr : ~p", [Nr]),
    NrList = lists:seq(1, Nr),
    
    lists:foreach(fun(X)->
			  InstName = 
			      atom_to_list(NC_hookName)++
			      "_"++integer_to_list(X),
			  ok = rct_nc_testclass1_lib:
			      nc_delete_mo_instance(NC_hookName, 
						    InstName)
		  end, NrList),

    ct:pal("Delete Done.", []).

%%--------------------------------------------------------------------
%% @doc 
%% Open netconf sessions. <br/>
%% @end
%%--------------------------------------------------------------------
nc_open_session(NC_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  ct:pal("### Open Name: ~p", [Name]),
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
			  ct:pal("### CloseName: ~p", [Name]),
			  ok = ct_netconfc:close_session(Name)
    		  end, 
    		  NC_SessionNameList),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% check_notifications. <br/>
%% @end
%%--------------------------------------------------------------------
check_notifications(Nr, SessionForCreate, SessionsForSub) ->
    %%
    %% Wait on notification from add request.
    %%
    ct:pal("Start Check of notification messages"),
    lists:foreach(fun(X) ->
			  InstName = 
			      atom_to_list(SessionForCreate)++
    			      "_"++integer_to_list(X),
    			  AttrName = integer_to_list(X),
    			  lists:
			      foreach(fun(Session) ->
					      receive
						  {notification,
						   [{xmlns,_}],
						   [{eventTime,[],_},
						    {events,[{xmlns,_},
							     {dnPrefix,[]}],
						     [{objectCreated,
						       [{dn,"ManagedElement=1,TestRoot=1,TestClass1="++InstName}],
						       [{attr,
							 [{name,"int32"}],
							 [{v,[],[AttrName]}]} | _]
						      }]
						    }]
						  } ->
						      ct:log("Netconf: Received expected objectCreated notification. "
							     "NC sess: ~p, MO inst: ~p", [Session, InstName])
					      after 20000 -> 
						      %% ct:pal("Error no notification within time! : ~p, ~p", 
						      %%        [Session, InstName]),
						      %% ct:fail("Error no notification within time!", [])
						      ct:pal("Error no notification within time! : ~p, ~p", [Session, InstName])
					      end
				      end, SessionsForSub)
    		  end, lists:seq(1,Nr)),
    ct:pal("End Check of notification messages").

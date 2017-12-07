%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	com_avc_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R4A/R8A/1
%%%
%%% @doc == Basics tests for new finctionality in COM and OAM ordered by RCS.==
%%% This Test Suite can be used on target and simulated enviroment.<br/>
%%%
%%%
%%% @end

-module(com_avc_SUITE).
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
%%% R2A/1      2013-06-19 etxpejn     Created
%%% R2A/3      2013-10-01 etxivri     Changed cli command from end to top, due to changes in com bd8.
%%% R2A/4      2013-11-12 etxivri     ChangedUpdate to use TestClass1 in FAKE instead of RbsUnit in FEQM.
%%% R4A/1      2015-05-12 etxarnu     Adapatation to COM 5.1
%%% R4A/2      2015-05-12 etxarnu     Adapatation to COI-based AVC handling
%%% R4A/3      2015-05-16 erarafo     Checking the COMSA version before running test cases
%%% R4A/4      2015-07-13 etxjovp     Add group definitions used by CS CI
%%% R8A/1      2016-12-12 etxkols     Removing rs232 hook
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
	 com_notification_at_create/1,
	 com_notification_at_delete/1,
	 com_notification_at_change/1,
	 get_comsa_version/1
	]).

-define(NrOfNetConfUsers, 3).
-define(NrOfCliUsers, 1).

-define(NC_SessionNameList, [list_to_atom("nc"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfNetConfUsers)]).
-define(CLI_SessionName, [list_to_atom("cli"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfCliUsers)]).

-define(NC_INT32, "1234").
-define(CLI_INT32, "5678").


%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% runs the rct_netconf hook for each user, to be able to open a netconf session for each user.<br/>
%% @end
%%--------------------------------------------------------------------
suite() ->
    %% build up a list of NetconfHooks touples with differents Names.
    NetconfHooks = [{rct_netconf, list_to_atom("nc"++integer_to_list(N))} || N <- lists:seq(1,?NrOfNetConfUsers)],
    CliHooks = [{rct_cli, {list_to_atom("cli"++integer_to_list(N)), [manual_connect]}} || N <- lists:seq(1,?NrOfCliUsers)],
    [{timetrap, {hours, 72}}, % 10 hours
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {cth_conn_log, []},
		 {rct_power,node},
		 {rct_core,[]},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}} |
		 NetconfHooks ++ CliHooks
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
	    %% Open a session and clean up testclass1, close all session.
            %%%%
	    try
		lists:foreach(fun(Name) -> nc_open(ct_netconfc:open(Name, []), Name) end,
       		      ?NC_SessionNameList)
	    catch throw:{?MODULE, found} ->
		    ok
	    end,
	    try
	    	lists:foreach(fun(Name) -> cli_connect(rct_cli:connect(Name, noprint), Name) end,
			      ?CLI_SessionName)
	    catch throw:{?MODULE, found} ->
	    	    ok
	    end,

            %%%%
	    %% Commit, close sessions.
            %%%%
	    lists:foreach(fun(Name1) ->
				  ct_netconfc:close_session(Name1)
			  end,
			  ?NC_SessionNameList),
	    lists:foreach(fun(Name1) ->
				  rct_cli:disconnect(Name1, noprint)
	    		  end,
	    		  ?CLI_SessionName)
    end,

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),
    ok.

nc_open({error,{connection_exists,_}}, Name) ->
    ct_netconfc:close_session(Name), % Clean up at our client, if netconf process was killed on node,
    ct_netconfc:open(Name, []), % Then set up again.
    nc_delete_testclass1(Name, ?NC_SessionNameList),
    throw({?MODULE, found});
nc_open({ok,_}, Name) ->
    nc_delete_testclass1(Name, ?NC_SessionNameList),
    throw({?MODULE, found});
nc_open(_, _) ->
    ok.

cli_connect({error,already_connected}, Name) ->
    rct_cli:disconnect(Name, noprint), % Clean up at our client, if cli process was killed on node,
    rct_cli:connect(Name, noprint), % Then set up again.
    cli_delete_testclass1(Name, ?CLI_SessionName),
    throw({?MODULE, found});
cli_connect(ok, Name) ->
    cli_delete_testclass1(Name, ?CLI_SessionName),
    throw({?MODULE, found});
cli_connect(_, _) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases defined in list.
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     get_comsa_version,
     com_notification_at_create,
     com_notification_at_delete,
     com_notification_at_change
    ].

%%--------------------------------------------------------------------
%% @doc
%% TCs runed by jenkins.
%% @end
%%--------------------------------------------------------------------
groups() ->
    AllGroup=all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},  
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},  
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},  
     {sdc__qual__all__1__group, [], [] },  
     {group_com_1,[],[
		      com_notification_at_create,
		      com_notification_at_delete,
		      com_notification_at_change
		     ]}
    ].


%%--------------------------------------------------------------------
%% @doc
%% Verify that the sessions that are subscribers on event notifications <br/>
%% for objectCreated on TestClass1.* will get the expected event after the create <br/>
%% request from both netconf and cli. <br/>
%% @spec com_notification_at_create(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
com_notification_at_create(_Config) ->
    ComsaVersion = getComsaVersion(),
    if
	ComsaVersion < "R4A06" ->
	    ct:pal("skipping test 'com_notification_at_create'", []),
	    ok;
	true ->
	    %%
	    %% Create netconf and cli sessions.
	    %%
	    ok = nc_open_session(?NC_SessionNameList),
	    ok = cli_create_connection(?CLI_SessionName),
	    CreateRequest = ["objectCreated"],
	    
	    SessionForCreate = lists:nth(1,?NC_SessionNameList),
	    SessionsForSub = ?NC_SessionNameList -- [SessionForCreate],
	    
	    lists:foreach(fun(Session) ->
				  ok = nc_add_sub(Session, [{["ManagedElement=1,TestRoot=1,TestClass1.*"],
							     CreateRequest}],[])
			  end, SessionsForSub),
	    
	    %%
	    %% Test that the notification will show if testclass1 is created via netconf
	    %%
	    ok = nc_add_testclass1(SessionForCreate),
	    
	    ct:pal("### Close session: ~p", [SessionForCreate]),
	    ok = ct_netconfc:close_session(SessionForCreate),
	    
	    %%
	    %% Wait on notification from add request.
	    %%
	    lists:foreach(fun(_Session) ->
				  receive
				      {notification,
				       [{xmlns,_}],
				       [{eventTime,[],_},
					{events,
					 [{xmlns,_},
					  {dnPrefix,[]}],
					 [{objectCreated,
					   [{dn,"ManagedElement=1,TestRoot=1,TestClass1=nc1"}],
					   [{attr,[{name,"struct1"}],[]},
					    {attr,
					     [{name,"int32"}],
					     [{v,[],[?NC_INT32]}]}]}]}]}=N ->
					  ct:pal("Netconf: Received expected objectCreated notification -~n~p", [N])
				  after 10000 ->
				      ct:fail("Error no notification within time!~n"
					      "Messages = ~p~n",
					      [erlang:process_info(self(),messages)])
				  end
			  end, SessionsForSub),
	    
	    ct:pal("### Open session: ~p", [SessionForCreate]),
	    {ok,_} = ct_netconfc:open(SessionForCreate,[]),
	    
	    ok = nc_delete_testclass1(SessionForCreate),
	    
	    %%
	    %% Test that the notification will show if testclass1 is created via cli
	    %%
	    ok = cli_add_testclass1(?CLI_SessionName),
	    %%
	    %% Wait on notification from add request.
	    %%
	    lists:foreach(fun(_Session) ->
				  receive
				      {notification,
				       [{xmlns,_}],
				       [{eventTime,[],_},
					{events,[{xmlns,_},{dnPrefix,[]}],
					 [{objectCreated,
					   [{dn,"ManagedElement=1,TestRoot=1,TestClass1=cli1"}],
					   [{attr, [{name, "struct1"}], []},
					    {attr, [{name, "int32"}], []}]}]}]}
					% =N
					->
					  %ct:pal("CLI: Received expected objectCreated notification -~n~p", [N]),
					  ct:pal("CLI: Received expected objectCreated notification")
				  after 10000 ->
				      ct:fail("Error no notification within time!", [])
				  end
			  end, SessionsForSub),
	    
	    ok = cli_delete_testclass1(?CLI_SessionName),
	    
	    ok = nc_close_session(?NC_SessionNameList),
	    ok = cli_remove_connection(?CLI_SessionName),
	    ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Verify that the sessions that are subscribers on event notifications <br/>
%% for objectDeleted on testclass1.* will get the expected event after the delete <br/>
%% request from both netconf and cli. <br/>
%% @spec com_notification_at_delete(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
com_notification_at_delete(_Config) ->
    
    ComsaVersion = getComsaVersion(),
    if
	ComsaVersion < "R4A06" ->
	    ct:pal("skipping test 'com_notification_at_create'", []),
	    ok;
	true ->
	    
	    
	    %%
	    %% Create netconf and cli sessions.
	    %%
	    ok = nc_open_session(?NC_SessionNameList),
	    ok = cli_create_connection(?CLI_SessionName),
	    
	    DeleteRequest = ["objectDeleted"],
	    
	    SessionForDelete = lists:nth(1,?NC_SessionNameList),
	    SessionsForSub = ?NC_SessionNameList -- [SessionForDelete],
	    
	    lists:foreach(fun(Session) ->
				  ok = nc_add_sub(Session, [{["ManagedElement=1,TestRoot=1,TestClass1.*"],
							     DeleteRequest}], [])
			  end, SessionsForSub),
	    %%
	    %% Test that the notification will show if testclass1 is deleted via netconf
	    %%
	    ok = nc_add_testclass1(SessionForDelete),
	    
	    ct:pal("### Close session: ~p", [SessionForDelete]),
	    ok = ct_netconfc:close_session(SessionForDelete),
	    
	    ct:pal("### Open session: ~p", [SessionForDelete]),
	    {ok,_} = ct_netconfc:open(SessionForDelete,[]),
	    
	    ok = nc_delete_testclass1(SessionForDelete),
	    ct:pal("### Close session: ~p", [SessionForDelete]),
	    ok = ct_netconfc:close_session(SessionForDelete),
	    %%
	    %% Wait on notification from delete request.
	    %%
	    lists:foreach(fun(_Session) ->
				  receive
				      {notification,
				       [{xmlns,_}],
				       [{eventTime,[],_},
					{events,
					 [{xmlns,_},{dnPrefix,[]}],
					 [{objectDeleted,
					   [{dn,"ManagedElement=1,TestRoot=1,TestClass1=nc1"}],
					   []}]}]} ->
					  ct:pal("Netconf: Received expected objectDeleted notification")
				  after 10000 ->
				      ct:fail("Error no notification within time!", [])
				  end
			  end, SessionsForSub),
	    
	    %%
	    %% Test that the notification will show if testclass1 is deleted via cli
	    %%
	    ok = cli_add_testclass1(?CLI_SessionName),
	    ok = cli_delete_testclass1(?CLI_SessionName),
	    %%
	    %% Wait on notification from delete request.
	    %%
	    lists:foreach(fun(_Session) ->
				  receive
				      {notification,
				       [{xmlns,_}],
				       [{eventTime,[],_},
					{events,[{xmlns,_},{dnPrefix,[]}],
					 [{objectDeleted,
					   [{dn,"ManagedElement=1,TestRoot=1,TestClass1=cli1"}],
					   []}]}]} ->
					  ct:pal("CLI: Received expected objectDeleted notification")
				  after 10000 ->
				      ct:fail("Error no notification within time!", [])
				  end
			  end, SessionsForSub),
	    
	    ok = nc_close_session(?NC_SessionNameList -- [SessionForDelete]),
	    ok = cli_remove_connection(?CLI_SessionName),
	    ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Verify that a session that is a subscribes on event notifications <br/>
%% for attributeChanged on testclass1.* will get the expected event after <br/>
%% a attribute has been changed from both netconf and cli. <br/>
%% @spec com_notification_at_change(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
com_notification_at_change(_Config) ->
    ComsaVersion = getComsaVersion(),
    if
	ComsaVersion < "R4A06" ->
	    ct:pal("skipping test 'com_notification_at_create'", []),
	    ok;
	true ->
	    %%
	    %% Create netconf and cli sessions.
	    %%
	    ok = nc_open_session(?NC_SessionNameList),
	    ok = cli_create_connection(?CLI_SessionName),
	    
	    AttributRequest = ["attributeChanged"],
	    
	    SessionForChange = lists:nth(1,?NC_SessionNameList),
	    SessionsForSub = ?NC_SessionNameList -- [SessionForChange],
	    
	    lists:foreach(fun(Session) ->
				  ok = nc_add_sub(Session, [{["ManagedElement=1,TestRoot=1,TestClass1.*"],
							     AttributRequest}], [])
			  end, SessionsForSub),
	    %%
	    %% Test that the notification will show if testclass1 is changed via netconf
	    %%
	    ok = nc_add_testclass1(SessionForChange),
	    
	    ct:pal("### Close session: ~p", [SessionForChange]),
	    ok = ct_netconfc:close_session(SessionForChange),
	    
	    ct:pal("### Open session: ~p", [SessionForChange]),
	    {ok,_} = ct_netconfc:open(SessionForChange,[]),
	    
	    ok = nc_change_testclass1(SessionForChange, "4321"),
	    
	    ct:pal("### Close session: ~p", [SessionForChange]),
	    ok = ct_netconfc:close_session(SessionForChange),
	    %%
	    %% Wait on notification from change request.
	    %%
	    lists:foreach(fun(_Session) ->
				  receive
				      {notification,
				       [{xmlns,_}],
				       [{eventTime,[],_},
					{events,
					 [{xmlns,_},{dnPrefix,[]}],
					 [{'AVC',
					   [{dn,"ManagedElement=1,TestRoot=1,TestClass1=nc1"}],
					   [{attr,
					     [{name,"int32"}],
					     [{v,[],["4321"]}]}]}]}]} ->
					  ct:pal("Netconf: Received expected AVC notification")
				  after 10000 ->
				      ct:fail("Error no notification within time!", [])
				  end
			  end, SessionsForSub),
	    ct:pal("### Open session: ~p", [SessionForChange]),
	    {ok,_} = ct_netconfc:open(SessionForChange,[]),
	    
	    ok = nc_delete_testclass1(SessionForChange),
	    %%
	    %% Test that the notification will show if testclass1 is changed via cli
	    %%
	    ok = cli_add_testclass1(?CLI_SessionName),
	    ok = cli_change_testclass1(?CLI_SessionName, "8765"),
	    %%
	    %% Wait on notification from delete request.
	    %%
	    lists:foreach(fun(_Session) ->
				  receive
				      {notification,
				       [{xmlns,_}],
				       [{eventTime,[],_},
					{events,
					 [{xmlns,_},{dnPrefix,[]}],
					 [{'AVC',
					   [{dn,"ManagedElement=1,TestRoot=1,TestClass1=cli1"}],
					   [{attr,
					     [{name,"int32"}],
					     [{v,[],["8765"]}]}]}]}]} ->
					  ct:pal("CLI: Received expected AVC notification")
				  after 10000 ->
				      ct:fail("Error no notification within time!", [])
				  end
			  end, SessionsForSub),
	    
	    ok = cli_delete_testclass1(?CLI_SessionName),
	    
	    ok = nc_close_session(?NC_SessionNameList),
	    ok = cli_remove_connection(?CLI_SessionName),
	    ok
    end.


%%--------------------------------------------------------------------
%% @doc Report the COMSA version.
%% @end
%%--------------------------------------------------------------------
get_comsa_version(_Config) ->
    Version = getComsaVersion(),
    ct:pal("COMSA version is: ~s", [Version]),
    ok.


getComsaVersion() ->    
    VersionDir = rct_rpc:call(rpc, sysEnv, releases_vsn_dir, [], 10000),
    WildcardName = rct_rpc:call(rpc, filename, join, [VersionDir, "*.rel"], 10000),
    [PathName] = rct_rpc:call(rpc, filelib, wildcard, [WildcardName], 10000),
    VersionInfo = rct_rpc:call(rpc, file, consult, [PathName], 10000),
    {ok,[{release, _, _, KeyList}]} = VersionInfo,
    {_, Version, _} = lists:keyfind(comsa, 1, KeyList),
    Version.


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
nc_add_testclass1(NC_hookName) ->
    TestClass1_Id = atom_to_list(NC_hookName),
    Int32 = ?NC_INT32,
    ct:pal("### Create TestClass1: ~p, Int32: ~p", [TestClass1_Id, Int32]),

    ok = ct_netconfc:edit_config(NC_hookName,running,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
						      [{managedElementId,[],["1"]},
						       {'TestRoot',
							[{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
							[{testRootId,[],["1"]},
							 {'TestClass1',
							  [],
							  [{'testClass1Id',[],[TestClass1_Id]},
							   {int32,[],[Int32]}
							  ]}]}]}, 30000),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% cli add testclass1. <br/>
%% @end
%%--------------------------------------------------------------------
cli_add_testclass1([CLI_SessionName]) ->
    TestClass1_Id = atom_to_list(CLI_SessionName),
    Int32 = ?CLI_INT32,
    ct:pal("### Add TestClass1: ~p with Int32: ~p.", [TestClass1_Id, Int32]),
    rct_cli:send(CLI_SessionName, "configure"),
    rct_cli:send(CLI_SessionName, "ManagedElement=1,TestRoot=1,TestClass1="++TestClass1_Id),
    rct_cli:send(CLI_SessionName, "commit"),
    rct_cli:send(CLI_SessionName, "top"),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Test COM AVC by changing the int32 attribut of a TestClass1 in FAKE.<br/>
%% @end
%%--------------------------------------------------------------------
nc_change_testclass1(NC_hookName, Type) ->
    TestClass1_Id = atom_to_list(NC_hookName),
    Int32 = ?NC_INT32,
    ct:pal("### Change attribute for testclass1 id: ~p, from Int32: ~p to ~p",
	   [TestClass1_Id, Int32, Type]),

    ok = ct_netconfc:edit_config(NC_hookName,running,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
						      [{managedElementId,[],["1"]},
						       {'TestRoot',
							[{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
							[{testRootId,[],["1"]},
							 {'TestClass1',
							  [{xmlns,"urn:com:ericsson:ecim:TESTMOM"},
							   {'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
							   {'nc:operation',"replace"}],
							  [{testClass1Id,[],[TestClass1_Id]},
							   {int32, [], [Type]}]}]}]}),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% cli change testclass1. <br/>
%% @end
%%--------------------------------------------------------------------
cli_change_testclass1([CLI_SessionName], Type) ->
    TestClass1_Id = atom_to_list(CLI_SessionName),
    Int32 = ?CLI_INT32,
    ct:pal("### Change testclass1 id: ~p from int32: ~p, to ~p.", [CLI_SessionName, Int32, Type]),
    rct_cli:send(CLI_SessionName, "configure"),
    rct_cli:send(CLI_SessionName, "ManagedElement=1,TestRoot=1,TestClass1="++TestClass1_Id),
    rct_cli:send(CLI_SessionName, "int32="++Type),
    rct_cli:send(CLI_SessionName, "commit"),
    rct_cli:send(CLI_SessionName, "top"),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Test netconf delete by deleteing a TestClass1 in FAKE.<br/><br/>
%% @end
%%--------------------------------------------------------------------
nc_delete_testclass1(NC_hookName) ->
    TestClass1_Id =atom_to_list(NC_hookName),
    Int32 = ?NC_INT32,
    ct:pal("### Delete TestClass1: ~p, int32: ~p", [TestClass1_Id, Int32]),

    ok = ct_netconfc:edit_config(NC_hookName,running,{'ManagedElement',
						      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
						      [{managedElementId,[],["1"]},
						       {'TestRoot',
							[{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
							[{testRootId,[],["1"]},
							 {'TestClass1',
							  [{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
							   {'nc:operation',"delete"}],
							  [{'testClass1Id',[],[TestClass1_Id]},
							   {int32,[],[Int32]} ]}]}]}, 30000),
    ok.
%%%%%%%%%%%%%%%%%
%% Use in cleanup
%% Need to open/close after each delete.
%%%%%%%%%%%%%%%%%
nc_delete_testclass1(NC_hookName, NC_SessionNameList) ->
    lists:foreach(fun(SesionName) ->
			  TestClass1_Id =atom_to_list(SesionName),
			  ct_netconfc:open(NC_hookName,[]),
			  ct:pal("### Delete testclass1 id: ~p.", [TestClass1_Id]),
			  ct_netconfc:edit_config(NC_hookName,running,{'ManagedElement',
								       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
								       [{managedElementId,[],["1"]},
									{'TestRoot',
									 [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
									 [{testRootId,[],["1"]},
									  {'TestClass1',
									   [{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
									    {'nc:operation',"delete"}],
									   [{'testClass1Id',[],[TestClass1_Id]}
									   ]}]}]}, 30000),
			  ct_netconfc:close_session(NC_hookName)
		  end,
		  NC_SessionNameList).

%%--------------------------------------------------------------------
%% @doc
%% cli delete TestClass1. <br/>
%% @end
%%--------------------------------------------------------------------
cli_delete_testclass1([CLI_SessionName]) ->
    TestClass1_Id = atom_to_list(CLI_SessionName),
    ct:pal("### Delete TestClass1: ~p", [TestClass1_Id]),
    rct_cli:send(CLI_SessionName, "configure"),
    rct_cli:send(CLI_SessionName, "ManagedElement=1,TestRoot=1"),
    rct_cli:send(CLI_SessionName, "no TestClass1="++TestClass1_Id),
    rct_cli:send(CLI_SessionName, "commit"),
    rct_cli:send(CLI_SessionName, "top"),
    ok.

cli_delete_testclass1(SessionName, CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  TestClass1_Id = atom_to_list(Name),
			  rct_cli:send(SessionName, "configure", noprint),
			  rct_cli:send(SessionName, "ManagedElement=1,TestRoot=1", noprint),
			  rct_cli:send(SessionName, "no TestClass1="++TestClass1_Id, noprint),
			  rct_cli:send(SessionName, "commit", noprint),
			  rct_cli:send(SessionName, "top", noprint)
		  end,
		  CLI_SessionNameList).

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
%% CLI struff
%%--------------------------------------------------------------------
%% @doc
%% cli create connection. <br/>
%% @end
%%--------------------------------------------------------------------
cli_create_connection(CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  ct:pal("### CLI Connect Name: ~p", [Name]),
			  ok = rct_cli:connect(Name)
		  end,
		  CLI_SessionNameList),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% cli remove connection. <br/>
%% @end
%%--------------------------------------------------------------------
cli_remove_connection(CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  ct:pal("### Disconnect: ~p", [Name]),
			  ok = rct_cli:disconnect(Name)
		  end,
		  CLI_SessionNameList),
    ok.

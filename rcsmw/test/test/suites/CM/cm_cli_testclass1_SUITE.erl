%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	cm_cli_testclass1_SUITE.erl %
%%% @author etxjovp
%%% @copyright Ericsson AB 2013-2015
%%% @version /main/R2A/R3A/2
%%%
%%% @doc == Test of differents CM operation via cli.==
%%% This Test Suite can be used on target enviroment.<br/>
%%%
%%%
%%% @end

-module(cm_cli_testclass1_SUITE).
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
%%% R2A/2      2013-12-05 etxivri     Created. TC that removes attributes of
%%%                                   differents types, struct and 
%%%                                   derivedDataType 
%%% R3A/1      2015-07-13 etxjovp     Add group definitions used by CS CI
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
	 cli_delete_struct_attr/1,
	 cli_delete_derived_datatype_attr/1
	]).

-define(NrOfCliUsers, 5).
-define(CLI_SessionNameList, 
	[list_to_atom(
	   "cli"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfCliUsers)]).

-define(ATTR, "12345").
-define(PRINT_OPT, print).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% runs the rct_cli hook for each user, to be able to use cli for each user.<br/>
%% @end
%%--------------------------------------------------------------------
suite() ->
    %% build up a list of CliHooks touples with differents Names.
    CliHooks = [{rct_cli, {list_to_atom("cli"++integer_to_list(N)), [manual_connect]}} || N <- lists:seq(1,?NrOfCliUsers)] ,
    %% ct:pal("CliHooks: ~p",[CliHooks]),
    [{timetrap, {minutes, 30}},
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 %% {cth_conn_log, []},
		 %% {rct_power,node},
		 %% {rct_consserv,cs1},
                 %% {rct_rs232,console},
		 {rct_core,[]},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT",
						"CRASH REPORT"],
					       []}}]}} |
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
    	    ct:pal("Testcase failed due to: ~p.  \nClean up added instances", 
		   [Reason]),

	    A = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 
			     5000, noprint),
	    B = rct_rpc:call(rpc, os, cmd, ["pgrep cli -u $USER"], 
			     5000, noprint),
	    BB = length(string:tokens(B,"\n")),
	    ct:pal("### Com: ~p",[A]),
	    ct:pal("### Cli: ~p",[BB]),

	    %%%%
	    %% Open a connection and clean up instances.
            %%%%
	    try
	    	lists:foreach(fun(Name) -> 
				      cli_connect(rct_cli:connect(Name, noprint)
						 , Name) 
			      end, ?CLI_SessionNameList)
	    catch throw:{?MODULE, found} ->
	    	    ok
	    end,

	    %%%%
	    %% Close connection
	    %%%%
	    lists:foreach(fun(Name1) ->
	    			  rct_cli:disconnect(Name1, noprint)
	    		  end,
	    		  ?CLI_SessionNameList)

    end,

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, print),

    ok.

cli_connect({error,already_connected}, Name) ->
    % Clean up at our client, if cli process was killed on node,
    rct_cli:disconnect(Name, noprint), 
    % Then set up again.
    rct_cli:connect(Name, noprint), 
    cli_delete_instances(Name, ?CLI_SessionNameList),
    throw({?MODULE, found});
cli_connect(ok, Name) ->
    cli_delete_instances(Name, ?CLI_SessionNameList),
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
     cli_delete_struct_attr,
     cli_delete_derived_datatype_attr
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
     {sbc__upgrade_short__all__1__group, [], [{group, default__group}]},  
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},  
     {sdc__qual__all__1__group, [], []}
    ].

%%--------------------------------------------------------------------
%% @doc
%%  CLI users add TestClass1 and attribute. <br/>
%%  Then delete attribute of type struct. <br/>
%%  The delete shall be OK.
%% @spec cli_delete_struct_attr(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_delete_struct_attr(_Config) ->
    %%%%
    %% Create cli connection.
    %%%%
    create_connection(?CLI_SessionNameList),

    %%%%
    %% Add instances and Attributes.
    %%%%
    add_testclass1(?CLI_SessionNameList),

    %%%%
    %% Remove struct attribute.
    %%%%
    delete_attr(?CLI_SessionNameList, "struct1"),

    %%%%
    %% Delete instances
    %%%%
    delete_testclass1(?CLI_SessionNameList),

    %%%%
    %% Close session
    %%%%
    remove_connection(?CLI_SessionNameList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%%  CLI users add TestClass1 and attribute. <br/>
%%  Then delete attribute of type derivedDataType. int32 is of that type. <br/>
%%  The delet shall be OK.
%% @spec cli_delete_derived_datatype_attr(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_delete_derived_datatype_attr(_Config) ->
    %%%%
    %% Create cli connection.
    %%%%
    create_connection(?CLI_SessionNameList),

    %%%%
    %% Add instances and Attributes.
    %%%%
    add_testclass1(?CLI_SessionNameList),

    %%%%
    %% Delete derivedDataType attribute.
    %% Int32 is of that type.
    %%%%
    delete_attr(?CLI_SessionNameList, "int32"),

    %%%%
    %% Delete instances
    %%%%
    delete_testclass1(?CLI_SessionNameList),

    %%%%
    %% Close session
    %%%%
    remove_connection(?CLI_SessionNameList),

    ok.


%%% Internal functions
%%--------------------------------------------------------------------
%% @doc
%% cli create connection. <br/>
%% @end
%%--------------------------------------------------------------------
create_connection(CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  ct:pal("### CLI Connect Name: ~p", [Name]),
			  ok = rct_cli:connect(Name)
		  end,
		  CLI_SessionNameList).

%%--------------------------------------------------------------------
%% @doc
%% cli remove connection. <br/>
%% @end
%%--------------------------------------------------------------------
remove_connection(CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  ct:pal("### Disconnect: ~p", [Name]),
			  ok = rct_cli:disconnect(Name)
		  end,
		  CLI_SessionNameList).

%%--------------------------------------------------------------------
%% @doc
%% cli add instances. <br/>
%% @end
%%--------------------------------------------------------------------
add_testclass1(CLI_SessionNameList) ->
    lists:foreach(fun(Name)->
			  InstName = atom_to_list(Name),
			  ct:pal("### Add instance: ~p with attr.", [Name]),
			  rct_cli_testclass1_lib:
			      add_mo_inst_and_attr(Name, 
						   InstName, 
						   ?ATTR, 
						   ?PRINT_OPT)
			      end, CLI_SessionNameList),
		  ok.
    
%%--------------------------------------------------------------------
%% @doc
%% cli delete instances. <br/>
%% @end
%%--------------------------------------------------------------------
delete_testclass1(CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  InstName = atom_to_list(Name),
			  ct:pal("### Delete instance: ~p", [Name]),
			  rct_cli_testclass1_lib:
			      delete_mo_inst(Name, InstName, ?PRINT_OPT)
		  end,
		  CLI_SessionNameList),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% delete_attr.   <br/>
%% @end
%%--------------------------------------------------------------------
delete_attr(CLI_SessionNameList, Attr) ->
    lists:
	foreach(
	  fun(Cli_session) ->
		  InstName = atom_to_list(Cli_session),
		  ct:pal("Delete attribute: ~p, in instance: ~p",[Attr, 
								  InstName]),
		  rct_cli:send(Cli_session, 
			       "configure", "\\(config\\)>", ?PRINT_OPT),
		  rct_cli:send(Cli_session, 
			       "ManagedElement=1,TestRoot=1,TestClass1="++
				   InstName, ?PRINT_OPT),
		  rct_cli:send(Cli_session, "no "++ Attr, 
			       "\\(config-TestClass1="++InstName++"\\)>",
			       ?PRINT_OPT), 
		  rct_cli:send(Cli_session, "commit",
			       "\\(TestClass1="++InstName++"\\)>", print),
		  rct_cli:send(Cli_session, "top", ">", ?PRINT_OPT)
	  end, CLI_SessionNameList),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% cli_delete_instances. Used in clanup if TC fails !<br/>
%% @end
%%--------------------------------------------------------------------
cli_delete_instances(SessionName, CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  InstName = atom_to_list(Name),
			  rct_cli_testclass1_lib:
			      delete_several_mo_inst(SessionName, [InstName])
		  end,
		  CLI_SessionNameList).

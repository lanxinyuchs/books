%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	measure_response_time_SUITE.erl %
%%% @author erarafo
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R5A/1
%%%
%%% @doc ==Measure responce times for differents netconf and cli operations.==
%%% This Test Suite can be used on target enviroment.
%%% Used sw version and measured data is written to a file.
%%% Path: /proj/rcs/measurements/
%%%
%%% <br/><br/>
%%%
%%% @end


-module(measure_response_time_SUITE).

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
%%% R2A/1      2013-03-12 etxivri     Created
%%% R2A/3      2013-03-15 etxivri     Changed a lot!
%%% R2A/4      2013-03-15 etxivri     Corrected  RESULTDIR!
%%% R2A/5      2013-10-01 etxivri     Changed cli command end to top, due to changes in com bd8.
%%% R5A/1      2016-01-21 erarafo     Fixed deprecation warnings and copyright
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
	 get_sw_version/0,
	 groups/0,
	 all/0,
	 measure_response_time_nc_operation_commit/1,
	 measure_response_time_cli_operation_commit/1
	]).

-define(NC_Name, nc1). %% NC hook name
-define(CLI_Name, cli1). %% CLI hook name

-define(RESULTDIR, "/proj/rcs/measurements/").
%% -define(RESULTDIR, "/home/etxivri/tmp/").

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {minutes, 600}}, % 10 hours
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {rct_netconf, nc1},
		 {cth_conn_log, []},
		 {rct_consserv,cs1},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
                 {rct_rs232,console},
		 {rct_cli, {cli1, [manual_connect]}},
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

	    %%%%%%%%%%%%%%
	    %% Clean up cli configurations!
	    %%%%%%%%%%%%%%
	    %%%%
	    %% Open a cli connection and clean up rbsUnits.
            %%%%
	    try
	    	cli_connect(rct_cli:connect(?CLI_Name, noprint), ?CLI_Name)
	    catch throw:{?MODULE, found} ->
	    	    ok
	    end,

	    %%%%
	    %% Close connection
	    %%%%
	    rct_cli:disconnect(?CLI_Name, noprint)

    end,

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 5000, noprint),

    ok.

nc_open({error,{connection_exists,_}}, Name) ->
    ct_netconfc:close_session(Name), % Clean up at our client, if netconf process was killed on node,
    ct_netconfc:open(Name, []), % Then set up again.
    nc_delete_rbsunit_no_check(Name),
    throw({?MODULE, found});
nc_open({ok,_}, Name) ->
    nc_delete_rbsunit_no_check(Name),
    throw({?MODULE, found});
nc_open(_, _) ->
    ok.

cli_connect({error,already_connected}, Name) ->
    rct_cli:disconnect(Name, noprint), % Clean up at our client, if cli process was killed on node,
    rct_cli:connect(Name, noprint), % Then set up again.
    cli_delete_rbsunit_no_check(Name, noprint),
    throw({?MODULE, found});
cli_connect(ok, Name) ->
    cli_delete_rbsunit_no_check(Name, noprint),
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


%% ===========================================================================
%% @doc
%% This will do netconf operationa and measure the response time. <br/>
%% Commmit (close) will be done after each operation except from get operation. <br/>
%% The measured data will be stored in file. <br/>
%% - open netconf. <br/>
%% - Create MO, rbsUnit.   <br/>
%% - Get MO, rbsUnit.   <br/>
%% - Add attribute, userlabel.  <br/>
%% - Get MO, rbsUnit, userlabel.   <br/>
%% - Change attribute in MO, change userlabel.  <br/>
%% - Delete MO, rbsUnit.  <br/>
%% - <br/>
%% @spec measure_response_time_nc_operation_commit(_Config) -> ok
%% @end
%% ===========================================================================
measure_response_time_nc_operation_commit(_Config) ->
    ct:pal("### Measure response time from differents nc opetaions. Not include open sesssions", []),

    SessionName = ?NC_Name,
    RbsUnitId = atom_to_list(SessionName),
    UserLabel = "userLabel_" ++ RbsUnitId,
    ChangedUserLabel = "changed_userLabel_" ++ RbsUnitId,

    %%%%
    %% Precondition is a open netconf sessions is up and running.
    %%%%
    ct:pal("### Open NC session, Name: ~p", [SessionName]),
    {ok,_} = ct_netconfc:open(SessionName,[]),

    %%%%
    %% Create rbsunit.
    %%%%
    ct:pal("### Create RBS Unit id: ~p, close session", [RbsUnitId]),
    Start1 = erlang:timestamp(),
    ok = nc_add_rbsunit(SessionName, RbsUnitId),
    ok = ct_netconfc:close_session(SessionName),
    End1 = erlang:timestamp(),

    %%%%
    %% Get rbsunit.
    %%%%
    {ok,_} = ct_netconfc:open(SessionName,[]),
    ct:pal("### Get config for RBS Unit id: ~p", [RbsUnitId]),
    Start2 = erlang:timestamp(),
    ok = nc_get_rbsunit(SessionName, RbsUnitId),
    End2 = erlang:timestamp(),

    %%%%
    %% Add userlabel.
    %%%%
    ct:pal("### Add userlabel: ~p, userlabel: ~p, close session",
	   [RbsUnitId, UserLabel]),
    Start3 = erlang:timestamp(),
    ok = nc_add_userlabel(SessionName, RbsUnitId, UserLabel),
    ok = ct_netconfc:close_session(SessionName),
    End3 = erlang:timestamp(),

    %%%%
    %% Get config for rbsunit, userlabel.
    %%%%
    {ok,_} = ct_netconfc:open(SessionName,[]),
    ct:pal("### Get config for RBS Unit id: ~p,  userlabel ~p", [RbsUnitId, UserLabel]),
    Start4 = erlang:timestamp(),
    ok = nc_get_rbsunit_userlabel(SessionName, RbsUnitId, UserLabel),
    End4 = erlang:timestamp(),

    %%%%
    %% Change attribute userlabel.
    %%%%
    ct:pal("### Change userlabel: ~p to userlabel: ~p, close session",
	   [UserLabel, ChangedUserLabel]),
    Start5 = erlang:timestamp(),
    ok = nc_change_userlabel(SessionName, RbsUnitId, ChangedUserLabel),
    ok = ct_netconfc:close_session(SessionName),
    End5 = erlang:timestamp(),

    %%%%
    %% Delete RbsUnit.
    %%%%
    {ok,_} = ct_netconfc:open(SessionName,[]),
    ct:pal("### Delete RBS Unit id: ~p, close session", [RbsUnitId]),
    Start6 = erlang:timestamp(),
    ok = nc_delete_rbsunit(SessionName, RbsUnitId),
    ok = ct_netconfc:close_session(SessionName),
    End6 = erlang:timestamp(),

    %%%%
    %% Response times in ms.
    %%%%
    NcAddRbsUnitTime = trunc(timer:now_diff(End1, Start1) / 1000),
    GetRbsUnitTime = trunc(timer:now_diff(End2, Start2) / 1000),
    NcAddUserLabelbuteTime = trunc(timer:now_diff(End3, Start3) / 1000),
    GetRbsUnitUserLabelTime = trunc(timer:now_diff(End4, Start4) / 1000),
    NcChangeUserLabelTime = trunc(timer:now_diff(End5, Start5) / 1000),
    NcDeleteRbsUnitTime = trunc(timer:now_diff(End6, Start6) / 1000),

    TotTime = NcAddRbsUnitTime+
	GetRbsUnitTime+
    	NcAddUserLabelbuteTime+
    	GetRbsUnitUserLabelTime+
    	NcChangeUserLabelTime+
    	NcDeleteRbsUnitTime,

    %%%%
    %% Get sw version, using cli
    %%%%
    CXS_label = get_sw_version(),

    %%%%
    %% Update log measurement file
    %%%%
    FileName = "measure_response_time_nc_operation_commit",
    updateMeasResFile(FileName, "~p;~w;~w;~w;~w;~w;~w;~w;~w~n",
    		      [httpd_util:rfc1123_date(),
    		       CXS_label,
		       NcAddRbsUnitTime,
		       GetRbsUnitTime,
		       NcAddUserLabelbuteTime,
		       GetRbsUnitUserLabelTime,
		       NcChangeUserLabelTime,
		       NcDeleteRbsUnitTime,
		       TotTime]),

    ct:pal("All results from executed runs is logged in \n file: ~p ,\n path: ~p \n Result output: Date,CXS,AddRbsUnitTime,GetRbsUnitTime,NcAddUserLabelbuteTime,GetRbsUnitUserLabelTime,NcChangeUserLabelTime,DeleteRbsUnitTime,TotTime \n",[FileName, ?RESULTDIR]),

    ct:pal("NcAddRbsUnitTime: ~w; GetRbsUnitTime: ~w;  NcAddUserLabelbuteTime: ~w; GetRbsUnitUserLabelTime: ~w; NcChangeUserLabelTime: ~w; NcDeleteRbsUnitTime: ~w; TotTime: ~w ~n",
    	   [NcAddRbsUnitTime,
	    GetRbsUnitTime,
	    NcAddUserLabelbuteTime,
	    GetRbsUnitUserLabelTime,
	    NcChangeUserLabelTime,
	    NcDeleteRbsUnitTime,
	    TotTime]),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    ok.


%% ===========================================================================
%% @doc
%% This will do cli operationa and measure the response time. <br/>
%% Commmit and end will be done after each operation. <br/>
%% The measured data will be stored in file. <br/>
%% - Open cli connection. <br/>
%% - Create rbsUnit.   <br/>
%% - show rbsUnit.   <br/>
%% - Add userlabel.  <br/>
%% - show rbsUnit, userlabel.   <br/>
%% - Change change userlabel.  <br/>
%% - Delete rbsUnit.  <br/>
%% - Close cli connection. <br/>
%% - <br/>
%% @spec measure_response_time_cli_operation_commit(_Config) -> ok
%% @end
%% ===========================================================================
measure_response_time_cli_operation_commit(_Config) ->
    ct:pal("### Measure response time from differents cli opetaions. Not include open cli sesssions", []),

    Name = ?CLI_Name,
    RbsUnitId = atom_to_list(Name),
    UserLabel = "userLabel_" ++ RbsUnitId,
    ChangedUserLabel = "changed_userLabel_" ++ RbsUnitId,

    %%%%
    %% Precondition is a open netconf sessions is up and running.
    %%%%
    ct:pal("### Open CLI Connection, Name: ~p", [Name]),
    ok = rct_cli:connect(Name),

    %%%%
    %% Add rbsunit.
    %%%%
    ct:pal("### Add RBS unit: ~p.", [RbsUnitId]),
    Start1 = erlang:timestamp(),
    ok = cli_add_rbsunit(Name, RbsUnitId),
    {ok , _} = rct_cli:send(Name, "commit"),
    {ok , _} = rct_cli:send(Name, "top"),
    End1 = erlang:timestamp(),

    %%%%
    %% Get rbsunit.
    %%%%
    ct:pal("### Get config for RBS Unit id: ~p", [RbsUnitId]),
    Start2 = erlang:timestamp(),
    ok = cli_show_rbsunit(Name, RbsUnitId),
    End2 = erlang:timestamp(),

    %%%%
    %% Add userlabel.
    %%%%
    ct:pal("### In RBS unit: ~p, add userlable: ~p.", [RbsUnitId, UserLabel]),
    Start3 = erlang:timestamp(),
    ok = cli_add_userlabel(Name, RbsUnitId, UserLabel),
    {ok , _} = rct_cli:send(Name, "commit"),
    {ok , _} = rct_cli:send(Name, "top"),
    End3 = erlang:timestamp(),

    %%%%
    %% Get config for rbsunit, userlabel.
    %%%%
    ct:pal("### Get config for RBS Unitid: ~p userlabel ~p", [RbsUnitId, UserLabel]),
    Start4 = erlang:timestamp(),
    ok = cli_show_rbsunit_userlabel(Name, RbsUnitId),
    End4 = erlang:timestamp(),

    %%%%
    %% Change attribute userlabel.
    %%%%
    ct:pal("### In RBS unit: ~p, change userlable to: ~p.", [UserLabel, ChangedUserLabel]),
    Start5 = erlang:timestamp(),
    ok = cli_change_userlabel(Name, RbsUnitId, ChangedUserLabel),
    {ok , _} = rct_cli:send(Name, "commit"),
    {ok , _} = rct_cli:send(Name, "top"),
    End5 = erlang:timestamp(),

    %%%%
    %% Delete RbsUnit.
    %%%%
    ct:pal("### Delete RBS units: ~p", [RbsUnitId]),
    Start6 = erlang:timestamp(),
    ok = cli_delete_rbsunit(Name, RbsUnitId),
    %% ok = cli_commit_if_wanted(Name, CommitOpt),
    {ok , _} = rct_cli:send(Name, "commit"),
    {ok , _} = rct_cli:send(Name, "top"),
    End6 = erlang:timestamp(),

    %%%%
    %% Response times in ms.
    %%%%
    %%
    AddRbsUnitTime = trunc(timer:now_diff(End1, Start1) / 1000),
    GetRbsUnitTime = trunc(timer:now_diff(End2, Start2) / 1000),
    AddUserLabelTime = trunc(timer:now_diff(End3, Start3) / 1000),
    GetRbsUnitUserLabelTime = trunc(timer:now_diff(End4, Start4) / 1000),
    ChangeUserLabelTime = trunc(timer:now_diff(End5, Start5) / 1000),
    DeleteRbsUnitTime = trunc(timer:now_diff(End6, Start6) / 1000),

    TotTime = AddRbsUnitTime+
	GetRbsUnitTime+
	AddUserLabelTime+
	GetRbsUnitUserLabelTime+
	ChangeUserLabelTime+
	DeleteRbsUnitTime,


    %%%%
    %% Close cli connection.
    %%%%
    ct:pal("### Close CLI connection, Name: ~p", [Name]),
    ok = rct_cli:disconnect(Name),
    %%%%
    %% Get sw version, using cli
    %%%%
    CXS_label = get_sw_version(),

    %%%%
    %% Update log measurement file
    %%%%
    FileName = "measure_response_time_cli_operation_commit",

    updateMeasResFile(FileName, "~p;~w;~w;~w;~w;~w;~w;~w;~w~n",
    		      [httpd_util:rfc1123_date(),
    		       CXS_label,
		       AddRbsUnitTime,
		       GetRbsUnitTime,
		       AddUserLabelTime,
		       GetRbsUnitUserLabelTime,
		       ChangeUserLabelTime,
		       DeleteRbsUnitTime,
		       TotTime ]),

    ct:pal("All results from executed runs is logged in \n file: ~p ,\n path: ~p \n Result output: Date,CXS,AddRbsUnitTime,ShowRbsUnitTime,AddAttributeTime,ShowRbsUnitUserLabelTime,ChangeAttributeTime,DeleteRbsUnitTime,CloseTime,TotTime \n",[FileName, ?RESULTDIR]),

    ct:pal("CliAddRbsUnitTime: ~w; ShowRbsUnitTime: ~w; CliAddAttributeTime ~w; ShowRbsUnitUserLabelTime: ~w; CliChangeAttributeTime ~w; CliDeleteRbsUnitTime:  ~w; TotTime: ~w ~n",
    	   [AddRbsUnitTime,
	    GetRbsUnitTime,
	    AddUserLabelTime,
	    GetRbsUnitUserLabelTime,
	    ChangeUserLabelTime,
	    DeleteRbsUnitTime,
	    TotTime]),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    ok.


%% NC funtions
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @hidden
%% nc_add_rbsunit(SessionName, RbsUnitId)
%%--------------------------------------------------------------------
nc_add_rbsunit(NC_hookName, RbsUnitId) ->
    ok = ct_netconfc:edit_config(NC_hookName,running,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
						      [{managedElementId,[],["1"]},
						       {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
							[{'equipmentId',[],["1"]},
							 {'RbsUnit',[{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
							  [{rbsUnitId,[],[RbsUnitId]}
							  ]}]}]}),
    ok.

%%--------------------------------------------------------------------
%% @hidden
%% nc_add_attribute(NC_hookName, RbsUnitId, UserLabel)
%%--------------------------------------------------------------------
nc_add_userlabel(NC_hookName, RbsUnitId, UserLabel) ->
    ok = ct_netconfc:edit_config(NC_hookName,running,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
						      [{managedElementId,[],["1"]},
						       {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
							[{'equipmentId',[],["1"]},
							 {'RbsUnit',[{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
							  [{rbsUnitId,[],[RbsUnitId]},
							   {userLabel,[],[UserLabel]}
							  ]}]}]}),
    ok.

%%--------------------------------------------------------------------
%% @hidden
%% nc_change_attribute(NC_hookName, RbsUnitId, UserLabel)
%%--------------------------------------------------------------------
nc_change_userlabel(NC_hookName, RbsUnitId, UserLabel) ->
    ok = ct_netconfc:edit_config(NC_hookName,running,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
						      [{managedElementId,[],["1"]},
						       {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
							[{'equipmentId',[],["1"]},
							 {'RbsUnit',[{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
							  [{rbsUnitId,[],[RbsUnitId]},
							   {userLabel,[],[UserLabel]}
							  ]}]}]}),
    ok.

%%--------------------------------------------------------------------
%% @hidden
%% nc_get_config_rbsunit(NC_hookName, RbsUnitId)
%%--------------------------------------------------------------------
nc_get_rbsunit(NC_hookName, RbsUnitId) ->

    {ok,[{'ManagedElement',
	  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	  [{managedElementId,[],["1"]},
	   {'Equipment',
	    [{xmlns,
	      "urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
	    [{equipmentId,[],["1"]},
	     {'RbsUnit',
	      [{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
	      [{rbsUnitId,[],[RbsUnitId]},
	       %% {userLabel,[],[]},
	       {hwInstallStatus,[], [_]},
	       {administrativeState,[], [_]},
	       {operationalState,[], [_]},
	       {availState,[], [_]},
	       {detectedProductData, _, _},
	       {operationalLed, _, _},
	       {faultLed, _, _},
	       {statusLed, _, _},
	       {maintenanceLed, _, _}]
	     }]}]}]} =
	ct_netconfc:get(NC_hookName, {'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				      [{managedElementId,[],["1"]},
				       {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
					[{'equipmentId',[],["1"]},
					 {'RbsUnit',  [{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
					  [{rbsUnitId,[],[RbsUnitId]}]}
					]}]}),

    ok.

%%--------------------------------------------------------------------
%% @hidden
%% nc_get_config_rbsunit_userlabel(NC_hookName, RbsUnitId, UserLabel)
%%--------------------------------------------------------------------
nc_get_rbsunit_userlabel(NC_hookName, RbsUnitId, UserLabel) ->
    {ok,[{'ManagedElement',
	  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	  [{managedElementId,[],["1"]},
	   {'Equipment',
	    [{xmlns,
	      "urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
	    [{equipmentId,[],["1"]},
	     {'RbsUnit',
	      [{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
	      [{rbsUnitId,[],[RbsUnitId]},
	       {userLabel,[],[UserLabel]}]
	     }]}]}]} =
	ct_netconfc:get(NC_hookName, {'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				      [{managedElementId,[],["1"]},
				       {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
					[{'equipmentId',[],["1"]},
					 {'RbsUnit',  [{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
					  [{rbsUnitId,[],[RbsUnitId]},
					   {userLabel,[],[UserLabel]}]}
					]}]}),

    ok.

%%--------------------------------------------------------------------
%% @hidden
%% nc_delete_rbsunit(NC_hookName, RbsUnitId)
%%--------------------------------------------------------------------
nc_delete_rbsunit(NC_hookName, RbsUnitId) ->
    ok = ct_netconfc:edit_config(NC_hookName,running,{'ManagedElement',
						      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
						      [{managedElementId,[],["1"]},
						       {'Equipment',
							[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
							[{'equipmentId',[],["1"]},
							 {'RbsUnit',
							  [{xmlns,"urn:com:ericsson:ecim:rbsunit"},
							   {'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
							   {'nc:operation',"delete"}],
							  [{rbsUnitId,[],[RbsUnitId]}
							  ]}]}]}),
    ok.

%%--------------------------------------------------------------------
%% @hidden
%% Used in cleanup if tc fail
%% nc_delete_rbsunit_no_check(NC_hookName)
%%--------------------------------------------------------------------
nc_delete_rbsunit_no_check(NC_hookName) ->
    RbsUnitId =atom_to_list(NC_hookName),
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
						     [{rbsUnitId,[],[RbsUnitId]}
						     ]}]}]}).

%% CLI stuff
%%--------------------------------------------------------------------
%% @hidden
%% cli_add_rbsunit(Name, RbsUnitId)
%%--------------------------------------------------------------------
cli_add_rbsunit(Name, RbsUnitId) ->
    {ok , _} = rct_cli:send(Name, "configure"),
    {ok , _} = rct_cli:send(Name, "ManagedElement=1,Equipment=1,RbsUnit="++RbsUnitId),

    ok.

%%--------------------------------------------------------------------
%% @hidden
%% cli_add_attribute(Name, RbsUnitId, UserLabel)
%%--------------------------------------------------------------------
cli_add_userlabel(Name, RbsUnitId, UserLabel) ->
    {ok , _} = rct_cli:send(Name, "configure"),
    {ok , _} = rct_cli:send(Name, "ManagedElement=1,Equipment=1,RbsUnit="++RbsUnitId++",userLabel="++UserLabel),

    ok.

%%--------------------------------------------------------------------
%% @hidden
%% cli_change_attribute(Name, RbsUnitId, UserLabel)
%%--------------------------------------------------------------------
cli_change_userlabel(Name, RbsUnitId, UserLabel) ->
    {ok , _} = rct_cli:send(Name, "configure"),
    {ok , _} = rct_cli:send(Name, "ManagedElement=1,Equipment=1,RbsUnit="++RbsUnitId++",userLabel="++UserLabel),

    ok.
%%--------------------------------------------------------------------
%% @hidden
%% cli_show_rbsunit(Name, RbsUnitId)
%%--------------------------------------------------------------------
cli_show_rbsunit(Name, RbsUnitId) ->
    {ok ,_RecievedData} = rct_cli:send(Name, "show ManagedElement=1,Equipment=1,RbsUnit="++RbsUnitId),
    ok.

%%--------------------------------------------------------------------
%% @hidden
%% cli_show_rbsunit_userlabel(Name, RbsUnitId)
%%--------------------------------------------------------------------
cli_show_rbsunit_userlabel(Name, RbsUnitId) ->
    {ok ,_RecievedData} = rct_cli:send(Name, "show ManagedElement=1,Equipment=1,RbsUnit="++RbsUnitId++"userLabel"),
    ok.

%%--------------------------------------------------------------------
%% @hidden
%% cli_delete_rbsunit(Name, RbsUnitId)
%%--------------------------------------------------------------------
cli_delete_rbsunit(Name, RbsUnitId) ->
    {ok , _} = rct_cli:send(Name, "configure"),
    {ok , _} = rct_cli:send(Name, "ManagedElement=1,Equipment=1"),
    {ok , _} = rct_cli:send(Name, "no RbsUnit="++RbsUnitId),

    ok.

%%--------------------------------------------------------------------
%% @hidden
%% Used in cleanup if tc fail
%% cli_delete_rbsunit_no_check(Name, PrintOpt)
%%--------------------------------------------------------------------
cli_delete_rbsunit_no_check(Name, PrintOpt) ->
    RbsUnitId = atom_to_list(Name),
    rct_cli:send(Name, "configure", PrintOpt),
    rct_cli:send(Name, "ManagedElement=1,Equipment=1", PrintOpt),
    rct_cli:send(Name, "no RbsUnit="++RbsUnitId, PrintOpt),
    rct_cli:send(Name, "commit"),
    rct_cli:send(Name, "top").

%% ===========================================================================
%% @doc
%% Get CXS label. <br/>
%% Get SW version using COM cli interface. <br/>
%% @spec get_sw_version() -> 'CXS_LABEL'
%% @end
%% ===========================================================================
get_sw_version() ->
    ct:pal("### Get SW version",[]),
    CLI_Name = ?CLI_Name,
    ok = rct_cli:connect(CLI_Name),
    {ok ,RecievedData} = rct_cli:send(CLI_Name,"show ManagedElement=1,SystemFunctions=1,SwM=1"),
    %% ct:pal("RecievedData: ~p", [RecievedData]),

    %% Clean upp RecievedData string to a list of strings.
    Var = string:tokens(RecievedData, "=\r\n "),
    %% drop data until you reach "SwVersionMain", the wanted CXS label is the second element.
    [_, CXS | _ ] = lists:dropwhile(fun(X) ->
					    X =/= "SwVersionMain"
				    end, Var),
    ct:pal("CXS: ~p", [CXS]),
    ok = rct_cli:disconnect(CLI_Name),

    list_to_atom(CXS).


%% ===========================================================================
%% @hidden
%% ===========================================================================
%% @doc
%% For each testcase to use for writing testcase results and/or store measurement results <br/>
%% into a common result file. <br/>
%% @spec updateMeasResFile(FileName, PrintOpts, MeasData) -> ok
%% @end
%% ===========================================================================
updateMeasResFile(FileName, PrintOpts, MeasData) ->
    {ok, FD} = file:open(?RESULTDIR ++ "/" ++ FileName, [append]),
    %% ct:pal("~p",[PrintOpts]),
    case length(ct:get_config(test_nodes)) of
    	1 ->
    	    [{_, NodeName}] = ct:get_config(test_nodes);
    	_ ->
    	    NodeName = dummy,
    	    ct:fail("TC not updated for multinode!")
    end,

    MeasInfo = MeasData++[NodeName],
    ct:pal("MeasInfo: ~p",[MeasInfo]),
    %% insert a ;~w before ~n due to the field NodeName.
    CompletePrintOpts = re:replace(PrintOpts, "~n", ";~w~n", [{return, list}]),
    %% ct:pal("CompletePrintOpts: ~p",[CompletePrintOpts]),
    io:format(FD, CompletePrintOpts, MeasInfo),

    file:close(FD),
    ok.

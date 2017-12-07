%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	nc_operations_SUITE.erl %
%%% @author erarafo
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R3A/R5A/1
%%%
%%% @doc ==Measure times for characteristics optimization.==
%%% <br/><br/>
%%%
%%% @end


-module(nc_operations_SUITE).

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
%%% R3A/2      2015-01-17 etxivri     Created
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

	 groups/0,
	 all/0,
	 %% %% get_eqm_mo/1,
	 %% %% get_eqm_userlabel_attr/1,
	 %% %% ct_get_eqm_mo/1,
	 %% %% ct_get_eqm_userlabel_attr/1,
	 %% get_enodebfunc_attr/1,
	 ct_get_enodebfunc_attr/1,
	 ct_set_enodebfunc_attr/1,
	 create_delete_mo_many_times/1,
	 ct_get_extnodebfunc_mo/1,
	 create_extnodebfunc_mo/1,
	 delete_extnodebfunc_mo/1,
	 gte_meausures/1
	]).

%% -define(RESULTDIR,"/home/etxivri/Kista/rcs_ithub/egenskaper/matningar_tmp/").
%% -define(Nr, 10).
-define(Nr, 2).
-define(NC_Sess, nc1).
-define(Cli_Sess, cli).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {minutes, 600}}, % 10 hours
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {rct_power,node},
		 {rct_netconf, nc1},
		 %% {cth_conn_log, []},
		 {rct_consserv,cs1},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT",
						"CRASH REPORT"],[]}}]}},
                 {rct_rs232,console},
		 {rct_cli, {cli, [manual_connect]}},
		 {rct_tlib,{load,[]}},
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
end_per_testcase(_TestCase, _Config) ->
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
%% TCs runed by jenkins.
%% @end
%%--------------------------------------------------------------------
groups() ->
    [
    ].

%% %% %%--------------------------------------------------------------------
%% %% %% @doc
%% %% %% @spec get_eqm_mo(_Config) -> ok
%% %% %% @end
%% %% %%--------------------------------------------------------------------
%% %% get_eqm_mo(_Config) ->
%% %%     GetEqmMoCmd = "/home/etxivri/Kista/rcs_ithub/egenskaper/boan/bra_o_ha_xml/get_equipment.xml ",
%% %%     NodeName = get_node_name(),
%% %%     ct:pal("Node:~n~p~n",[NodeName]),

%% %%     ct:pal("Start: Get Equipment MO, nr of times: ~p",[?Nr]),

%% %%     %% NrList = lists:seq(1,?Nr),
%% %%     %% Start1 = erlang:timestamp(),
%% %%     %% lists:foreach(fun(X) ->
%% %%     %% 			  %% ct:pal("Nr: ~p",[X]),
%% %%     %% 			  _Answ =
%% %%     %% 			      os:cmd("rcs_exec -m netconf -f "++
%% %%     %% 					  GetAttrCmd ++
%% %%     %% 					 NodeName)
%% %%     %% 			      end, NrList),

%% %%     %% End1 = erlang:timestamp(),

%% %%     TimeToGetEquipmentMo = generic_nc_operation(GetEqmMoCmd, ?Nr),

%% %%     ct:pal("End: Get Equipment MO,  nr of times: ~p",[?Nr]),

%% %%     %% TimeToCreate_Cell_MO = trunc(timer:now_diff(End1, Start1) / 1000),
%% %%     ct:pal("Time to Get Equipment MO: ~p ms. nr of times: ~p",
%% %% 	   [TimeToGetEquipmentMo, ?Nr]),

%% %%     ok.
%% %%     %% simple_check_of_nc_message(Answ).


%% %%--------------------------------------------------------------------
%% %% @doc
%% %% Note! open and close netconf session is in xml file.
%% %% @spec get_eqm_mo(_Config) -> ok
%% %% @end
%% %%--------------------------------------------------------------------
%% get_enodebfunc_attr(_Config) ->
%%     GetAttrCmd = "/home/etxivri/Kista/rcs_ithub/egenskaper/boan/RAST/get_enodebfunc_attr.xml ",
%%     NodeName = get_node_name(),
%%     ct:pal("Node:~n~p~n",[NodeName]),

%%     ct:pal("Start: Get Attribute, nr of times: ~p",[?Nr]),

%%     TimeToGetAttr = generic_nc_operation(GetAttrCmd, ?Nr),

%%     ct:pal("End: Get Attribute,  nr of times: ~p",[?Nr]),

%%     ct:pal("Time to Get Attribute: ~p ms. nr of times: ~p",
%% 	   [TimeToGetAttr, ?Nr]),

%%     ok.

%% generic_nc_operation(Cmd, NrOfTimes) ->
%%     NodeName = get_node_name(),
%%     ct:pal("Node:~n~p~n",[NodeName]),

%%     ct:pal("Start: Netconf Opeartion. Nr of times: ~p .", [NrOfTimes]),

%%     NrList = lists:seq(1, NrOfTimes),
%%     Start1 = erlang:timestamp(),

%%     lists:foreach(fun(_X) ->
%% 			  %% ct:pal("Nr: ~p",[X]),
%% 			  _Answ =
%% 			      os:cmd("rcs_exec -m netconf -f "++
%% 					 Cmd ++
%% 					 NodeName)
%% 		  end, NrList),

%%     End1 = erlang:timestamp(),

%%     ct:pal("End: Netconf Opeartion. Nr of times: ~p .", [NrOfTimes]),

%%     Time = trunc(timer:now_diff(End1, Start1) / 1000),
%%     ct:pal("Time for all operations: ~p ms.",[Time]).

%%--------------------------------------------------------------------
%% @doc
%% @spec ct_get_enodebfunc_attr(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
ct_get_enodebfunc_attr(_Config) ->
    NodeName = get_node_name(),
    ct:pal("Node:~n~p~n",[NodeName]),

    NrList = lists:seq(1, ?Nr),

    ct:pal("### Open NC session.", []),
    open(),

    ct:pal("Start: Get Attribute, nr of times: ~p",[?Nr]),

    Times = lists:map(fun(_X) ->
			      Start1 = erlang:timestamp(),

			      %% {ok,[{'ManagedElement',
			      %% 	    [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			      %% 	    [{managedElementId,[],["1"]},
			      %% 	     {'ENodeBFunction',
			      %% 	      [{xmlns,"urn:com:ericsson:ecim:Lrat"}],
			      %% 	      [{eNodeBFunctionId,[],["1"]},
			      %% 	       {zzzTemporary25,[],["-2000000000"]}]}]}]}
			      {ok, _}
				  = ct_netconfc:get(?NC_Sess,{'ManagedElement',
							 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
							 [{managedElementId,[],["1"]},
							  {'ENodeBFunction',
							   [{xmlns,"urn:com:ericsson:ecim:Lrat"}],
							   [{eNodeBFunctionId,[],["1"]},
							   {zzzTemporary25,[],[]}
							   ]}]}, 30000),

			      End1 = erlang:timestamp(),
			      %% timer:sleep(1000),

			      TimeToGetAttr =
				  trunc(timer:now_diff(End1, Start1) / 1000),
			      TimeToGetAttr
		      end, NrList),

    ct:pal("End: Get Attribute,  nr of times: ~p",[?Nr]),

    ct:pal("### Close NC session.", []),
    close(),

    TotTimeToGetAttribute = get_tot_times(Times),

    ct:pal("All: ~w~n", [Times]),
    ct:pal("TotTimeToReadAttribute: ~p ms ~n"
	   "Min: ~p ms ~n"
	   "Max: ~p ms ~n", [TotTimeToGetAttribute,
			    lists:min(Times),
			    lists:max(Times)]),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec ct_set_enodebfunc_attr(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
ct_set_enodebfunc_attr(_Config) ->
    NodeName = get_node_name(),
    ct:pal("Node:~n~p~n",[NodeName]),

    NrList = lists:seq(1, ?Nr),

    ct:pal("Start: Set Attribute, nr of times: ~p",[?Nr]),

    Times = lists:map(fun(X) ->
			      open(),

			      Start1 = erlang:timestamp(),

			      I = integer_to_list(X),
			      ok  = ct_netconfc:edit_config(?NC_Sess,
							    running,
							    {'ManagedElement',
							     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
							     [{managedElementId,[],["1"]},
							      {'ENodeBFunction',
							       [{xmlns,"urn:com:ericsson:ecim:Lrat"}],
							       [{eNodeBFunctionId,[],["1"]},
								{zzzTemporary25,[],[I]}
							       ]}]}, 30000),

			      close(),

			      End1 = erlang:timestamp(),
			      %% timer:sleep(2000),

			      TimeToSetAttr =
				  trunc(timer:now_diff(End1, Start1) / 1000),
			      TimeToSetAttr
		      end, NrList),

    ct:pal("End: Set Attribute,  nr of times: ~p",[?Nr]),

    TotTimeToSetAttribute = get_tot_times(Times),

    ct:pal("All: ~w~n", [Times]),
    ct:pal("TotTimeToSetAttribute: ~p ms ~n"
	   "Min: ~p ms ~n"
	   "Max: ~p ms ~n", [TotTimeToSetAttribute,
			    lists:min(Times),
			    lists:max(Times)]),

    ok.



%%--------------------------------------------------------------------
%% @doc
%% @spec create_delete_mo_many_times(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
create_delete_mo_many_times(_Config) ->
   NodeName = get_node_name(),
    ct:pal("Node:~n~p~n",[NodeName]),

    NrList = lists:seq(1, ?Nr),

    ct:pal("Start: Create /delete MO + Attribute, nr of times: ~p",[?Nr]),

    Times = lists:map(fun(_X) ->
                              %%%%
			      %% Create
			      %%%%
			      open(),
			      Start1 = erlang:timestamp(),

			      create_extenodebfunc_mo(),

			      close(),
			      End1 = erlang:timestamp(),

			      %% timer:sleep(2000),

			      TimeToCreateMo =
				  trunc(timer:now_diff(End1, Start1) / 1000),

			      %% %% Check
			      timer:sleep(1000),
			      ct_get_extnodebfunc_mo(ok),

			      %%%%
			      %% Delete
			      %%%%
			      open(),
			      Start2 = erlang:timestamp(),

			      delete_extenodebfunc_mo(),

			      close(),
			      End2 = erlang:timestamp(),

			      %% timer:sleep(2000),

			      TimeToDeleteMo =
				  trunc(timer:now_diff(End2, Start2) / 1000),

			      %% %% Check
			      timer:sleep(1000),
			      ct_get_extnodebfunc_mo(error),

			      {TimeToCreateMo,TimeToDeleteMo}
		      end, NrList),

    ct:pal("End: create / delete MO + Attribute,  nr of times: ~p",[?Nr]),

    ct:pal("Times : ~p",[Times]),
    {CreateTimes, DeleteTimes} = lists:unzip(Times),

    TotTimeToCreateMo= get_tot_times(CreateTimes),

    TotTimeToDeletMo= get_tot_times(DeleteTimes),

    ct:pal("All CreateTimes: ~w~n", [CreateTimes]),
    ct:pal("All DeleteTimes: ~w~n", [DeleteTimes]),

    ct:pal("TotTimeToCreateMo:~p~n"
	   "Min CreateTime:~p~n"
	   "Max CreateTime:~p~n",
	   [TotTimeToCreateMo,
	    lists:min(CreateTimes),
	    lists:max(CreateTimes)]),

    ct:pal("TotTimeToDeleteMo:~p~n"
	   "Min DeleteTime:~p~n"
	   "Max DeleteTime:~p~n",
	   [TotTimeToDeletMo,
	    lists:min(DeleteTimes),
	    lists:max(DeleteTimes)]),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec create_extnodebfunc_mo(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
create_extnodebfunc_mo(_Config) ->
    open(),
    A = erlang:timestamp(),
    create_extenodebfunc_mo(),
    close(),
    B = erlang:timestamp(),

    Time =
	trunc(timer:now_diff(B, A) / 1000),
    ct:pal("Time to create MO: ~p ms",[Time]),

    %% %% Check
    timer:sleep(1000),
    ct_get_extnodebfunc_mo(ok).

%%--------------------------------------------------------------------
%% @doc
%% @spec delete_extnodebfunc_mo(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
delete_extnodebfunc_mo(_Config) ->
    open(),
    A = erlang:timestamp(),
    delete_extenodebfunc_mo(),
    close(),
    B = erlang:timestamp(),
    Time =
	trunc(timer:now_diff(B, A) / 1000),
    ct:pal("Time to delete MO: ~p ms",[Time]),

    %% %% Check
    timer:sleep(1000),
    ct_get_extnodebfunc_mo(error).

%%--------------------------------------------------------------------
%% @doc
%% @spec gte_meausures(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
gte_meausures(_Config) ->
    %%%%
    %% Time to open session
    %%%%
    ct:log("Start ",[]),
    Start = erlang:timestamp(),
    open(),
    End = erlang:timestamp(),
    ct:log("End ",[]),

    TimeToOpen =
	trunc(timer:now_diff(End, Start) / 1000),
    ct:log("Time to open session: ~p ms",[TimeToOpen]),

    %%%%
    %% Get capabilities
    %%%%
    ct:log("Start 1",[]),
    Start1 = erlang:timestamp(),
    get_capabilities(),
    End1 = erlang:timestamp(),
    ct:log("End 1",[]),

    TimeToGetCap =
	trunc(timer:now_diff(End1, Start1) / 1000),
    ct:log("Get capabilities: ~p ms",[TimeToGetCap]),

    %%%%
    %% Get attribute userLabel in ExternalENodeBFunction MO
    %%%%
    ct:log("Start 2",[]),
    Start2 = erlang:timestamp(),
    get_attr_userlabel() ,
    End2 = erlang:timestamp(),
    ct:log("End 2",[]),

    TimeToGetUserLabel =
	trunc(timer:now_diff(End2, Start2) / 1000),
    ct:log("Get userlabel: ~p ms ",[TimeToGetUserLabel]),

    %%%%
    %% Get attribute swversion in swinventory MO.
    %%%%
    ct:log("Start 3",[]),
    Start3 = erlang:timestamp(),
    get_swversion(),
    End3 = erlang:timestamp(),
    ct:log("End 3",[]),

    TimeToGetSwVersion =
	trunc(timer:now_diff(End3, Start3) / 1000),
    ct:log("Get SwVersion: ~p ms",[TimeToGetSwVersion]),

    %%%%
    %% Time to close session
    %%%%
    ct:log("Start 4",[]),
    Start4 = erlang:timestamp(),
    close(),
    End4 = erlang:timestamp(),
    ct:log("End 4",[]),

    TimeToClose =
	trunc(timer:now_diff(End4, Start4) / 1000),
    ct:log("Time to close session: ~p ms",[TimeToClose]),

    ct:pal("Time to open session: ~p ms ~n"
	   "Time to get capabilities: ~p ms ~n"
	   "Time to get SwVersion: ~p ms ~n"
	   "Time to get userlabel: ~p ms ~n"
	   "Time to close session: ~p ms ~n",
	   [TimeToOpen,
	    TimeToGetCap,
	    TimeToGetSwVersion,
	    TimeToGetUserLabel,
	    TimeToClose]),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec ct_get_extnodebfunc_mo(Expect) -> ok
%% @end
%%--------------------------------------------------------------------
ct_get_extnodebfunc_mo(Expect) ->
    ct:log("### Open NC session.", []),
    open(),

    %% {ok,[{'ManagedElement',
    %% 	  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    %% 	  [{managedElementId,[],["1"]},
    %% 	   {'ENodeBFunction',
    %% 	    [{xmlns,"urn:com:ericsson:ecim:Lrat"}],
    %% 	    [{eNodeBFunctionId,[],["1"]},
    %% 	     {'EUtraNetwork',[],
    %% 	      [{eUtraNetworkId,[],["1"]},
    %% 	       {'ExternalENodeBFunction',[],
    %% 		[{eNodeBPlmnId,
    %% 		  [{struct,"PlmnIdentity"}],
    %% 		  [{mcc,[],["123"]},
    %% 		   {mnc,[],["456"]},
    %% 		   {mncLength,[],["3"]}]},
    %% 		 {externalENodeBFunctionId,[],["42"]},
    %% 		 {eNBId,[],["42"]},
    %% 		 {userLabel,[],[]},
    %% 		 {masterEnbFunctionId,[],[]},
    %% 		 {removingMonitoringStart,[],
    %% 		  [_]},
    %% 		 {ericssonENodeB,[],["false"]},
    %% 		 {mfbiSupport,[],["false"]}]}]}]}]}]}
    {Expect, _}
	= ct_netconfc:get(?NC_Sess,{'ManagedElement',
			       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			       [{managedElementId,[],["1"]},
				{'ENodeBFunction',
				 [{xmlns,"urn:com:ericsson:ecim:Lrat"}],
				 [{eNodeBFunctionId,[],["1"]},
				  {'EUtraNetwork',[],
				   [{eUtraNetworkId,[],["1"]},
				    {'ExternalENodeBFunction',[],
				     [{externalENodeBFunctionId,[],["42"]}
				     ]}]}]}]}, 30000),

    ct:log("### Close NC session.", []),
    close(),

    ok.

%% %% ===========================================================================
%% %% @doc
%% %% @end
%% %% ===========================================================================
%% create_external_enodeb_function_mo(_Config) ->



%% ===========================================================================
%% Internal.
%% ===========================================================================
open() ->
    {ok,_} = ct_netconfc:open(?NC_Sess,[]).


close()->
    ok = ct_netconfc:close_session(?NC_Sess).

get_tot_times(Times)->
    TotTimes = lists:foldl(fun(X, Sum) ->
				   X + Sum
			   end, 0, Times),
    TotTimes.

create_extenodebfunc_mo() ->
    ok  = ct_netconfc:edit_config(?NC_Sess,
				  running,
				  {'ManagedElement',
				   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				   [{managedElementId,[],["1"]},
				    {'ENodeBFunction',
				     [{xmlns,"urn:com:ericsson:ecim:Lrat"}],
				     [{eNodeBFunctionId,[],["1"]},
				      {'EUtraNetwork',[],
				       [{eUtraNetworkId,[],["1"]},
					{'ExternalENodeBFunction',[],
					 [{externalENodeBFunctionId,[],["42"]},
					  {eNodeBPlmnId,
					   [{struct,"PlmnIdentity"}],
					   [{mcc,[],["123"]},
					    {mnc,[],["456"]},
					    {mncLength,[],["3"]}]},
					  %% {userLabel,[], []},
					  {userLabel,[], ["1"]},
					  {masterEnbFunctionId,[],[]},
					  {mfbiSupport,[],["false"]},
					  {eNBId,[],["42"]}
					 ]}]}]}]}, 30000).


delete_extenodebfunc_mo() ->
    ok  = ct_netconfc:edit_config(?NC_Sess,
				  running,
				  {'ManagedElement',
				   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				   [{managedElementId,[],["1"]},
				    {'ENodeBFunction',
				     [{xmlns,"urn:com:ericsson:ecim:Lrat"}],
				     [{eNodeBFunctionId,[],["1"]},
				      {'EUtraNetwork',[],
				       [{eUtraNetworkId,[],["1"]},
					{'ExternalENodeBFunction',
					 [{'xmlns:nc',
					   "urn:ietf:params:xml:ns:netconf:"
					   "base:1.0"},
					  {'nc:operation',"delete"}],
					 [{externalENodeBFunctionId,[],["42"]}
					 ]}]}]}]}, 30000).


get_capabilities() ->
    %% ["urn:ietf:params:netconf:base:1.0","urn:com:ericsson:ebase:0.1.0",
    %%  "urn:com:ericsson:ebase:1.1.0","urn:com:ericsson:ebase:1.2.0",
    %%  "urn:ietf:params:netconf:capability:writable-running:1.0",
    %%  "urn:ietf:params:netconf:capability:rollback-on-error:1.0",
    %%  "urn:ietf:params:netconf:capability:notification:1.0",
    %%  "urn:ericsson:com:netconf:action:1.0"]
    [_|_]
	= ct_netconfc:get_capabilities(?NC_Sess).


get_attr_userlabel() ->
    %% {ok,[{'ManagedElement',
    %%                     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    %%                     [{managedElementId,[],["1"]},
    %%                      {'ENodeBFunction',
    %%                          [{xmlns,"urn:com:ericsson:ecim:Lrat"}],
    %%                          [{eNodeBFunctionId,[],["1"]},
    %%                           {'EUtraNetwork',[],
    %%                               [{eUtraNetworkId,[],["1"]},
    %%                                {'ExternalENodeBFunction',[],
    %%                                    [{externalENodeBFunctionId,[],["42"]},
    %%                                     {userLabel,[],[]}]}]}]}]}]}
    {ok,_}
	= ct_netconfc:get(?NC_Sess,{'ManagedElement',
			       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			       [{managedElementId,[],["1"]},
				{'ENodeBFunction',
				 [{xmlns,"urn:com:ericsson:ecim:Lrat"}],
				 [{eNodeBFunctionId,[],["1"]},
				  {'EUtraNetwork',[],
				   [{eUtraNetworkId,[],["1"]},
				    {'ExternalENodeBFunction',[],
				     [%%{externalENodeBFunctionId,[],["42"]},
				      {userLabel,[],[]}
				     ]}]}]}]}, 30000).


get_swversion() ->
    %% {ok,
    %%  [{'ManagedElement',
    %%    [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    %%    [{managedElementId,[],["1"]},
    %% 	{'SystemFunctions',[],
    %% 	 [{systemFunctionsId,[],["1"]},
    %% 	  {'SwInventory',
    %% 	   [{xmlns,"urn:com:ericsson:ecim:RcsSwIM"}],
    %% 	   [{swInventoryId,[],["1"]},
    %% 	    {'SwVersion',[],
    %% 	     [{swVersionId,[],["CXP9024418/1-R3B2391"]},
    %% 	      {administrativeData,
    %% 	       [{struct,"ProductData"}],
    %% 	       [{productName,[],["MSRBS"]},
    %% 		{productNumber,[],["CXP9024418/1"]},
    %% 		{productRevision,[],["R3B2391"]},
    %% 		{productionDate,[],["2015-01-21T12:27:15"]},
    %% 		{description,[],["N/A"]},
    %% 		{type,[],["MSRBS_V2"]}]},
    %% 	      {timeOfInstallation,[],["2015-01-22T08:59:00+00:00"]},
    %% 	      {timeOfActivation,[],["2015-01-22T08:59:41+00:00"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9024578/5-R60AY"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9023113/5-R1MA"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9024811/1-R4J"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP102171/1-R11A01"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9030284/3-R9DA"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9024940/1-R1A33"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9024578/9-R60AX"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9024578/12-R60AY"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9023064/4-R3NU"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9024263/1-R3A1038"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9024581/23-R23M"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9024020/1-R1X"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9024578/6-R60AX"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9024886/2-R2BE"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9022846/10-R13YN"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9024578/10-R60AY"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9024578/14-R60AY"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9023495/2-R1A606"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9024581/24-R24N"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9024812/1-R2N"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9024888/2-R8BE"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9026393/1-R1G"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9031275/3-R3A671"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9030859/1-R5SS"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9030699/2-R8FS"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9024280/4-R8DM"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9025671/23-R1A989"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9024813/1-R2E"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9025895/1-R1MK"]},
    %% 	      {consistsOf,[],
    %% 	       ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=CXP9024079/3-R1A37"]}]}]}]}]}]}
    {ok, _}
	= ct_netconfc:get(?NC_Sess,{'ManagedElement',
			       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			       [{managedElementId,[],["1"]},
				{'SystemFunctions',
				 [{systemFunctionsId,[],["1"]},
				  {'SwInventory',[],
				   [{swInventoryId,[],["1"]},
				    {'SwVersion',[],[]}
				   ]}]}]}, 30000).


get_node_name() ->
    [{_, NodeName}] = ct:get_config(test_nodes),
    Node = atom_to_list(NodeName),
    Node.

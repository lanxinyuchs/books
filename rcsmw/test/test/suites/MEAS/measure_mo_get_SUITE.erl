%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	measure_mo_get_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R3A/R6A/1
%%%
%%% @doc == Test Suite for testing netconf performance on large configs==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% @end

-module(measure_mo_get_SUITE).
-vsn('/main/R3A/R6A/1').
-author('etxkols').

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
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R3A/1      2015-05-29 etxjotj     Created
%%% R6A/1      2016-08-19 etxkols     Git migration requires that CC paths is not used 
%%%--------------------------------------------------------------------


%compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).
-export([simple_get/1]).


%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [simple_get].



%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf.
%% @end
%%--------------------------------------------------------------------
suite() ->
    case os:getenv("SIM_OR_TARGET") of
	"sim" -> 
	    [{timetrap, {minutes, 20}},
	     {ct_hooks, [{rct_htmllink,[]},
			 {rct_rpc, rpc_1},
			 {rct_netconf,{nc1, html}},
			 {cth_conn_log,[]},
			 {rct_core,[]},
			 {rct_logging,
			  {all, [{[erlang,swmLog],
			  {["ERROR REPORT","CRASH REPORT"],[]}}]}}
			]}];
	_Other ->
	     [{timetrap, {minutes, 20}},
	     {ct_hooks, 
	      case os:getenv("USER") of
		  "etxjotj" ->[];
		  _ -> 	[{rct_rs232, console}]
	      end++
		  [{rct_htmllink,[]},
		   {rct_rpc, rpc_1},
		   {rct_netconf,{nc1, html}},
		   {cth_conn_log,[]},
		   {rct_core,[]},
		   {rct_logging,
		    {all, [{[erlang,swmLog],
			    {["ERROR REPORT","CRASH REPORT"],[]}}]}}
		  ]}]
    end.

%% @hidden
init_per_suite(Config) ->
   MeId = case check_if_vc_board() of
	       "yes" -> ct:fail("vc_board");
	       _->
		   MeData = rct_rpc:call
			      (rpc_1, comsaI, get_managed_element_data, [], 
			       10000, noprint),
		   {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
		   net_kernel:disconnect(ErlNode),
		   case proplists:get_value(networkManagedElementId, MeData) of
		       undefined ->
			   "1";
		       NMEI -> NMEI
		   end
	   end,
    [{meId, MeId}|Config].

check_if_vc_board()->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
    Secure_board.

%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_testcase(TestCase, Config) ->
    ct:print("Now executing ~w~n",[TestCase]),
    Config.
%% @hidden
end_per_testcase(TestCase, _Config) ->
    ct:print("Test case ~w complete.~n",[TestCase]),
    ok.

%%--------------------------------------------------------------------
%% @doc Make a netconf get and count MOs
%% @end

simple_get(Config) ->
    MeId = proplists:get_value(meId, Config),
    {ok, _} = ct_netconfc:open(nc1, []),
    {Time, Reply} = timer:tc(ct_netconfc, get, 
    			     [nc1, [{'ManagedElement',[],
    				     [{managedElementId, [MeId]}]}]]),
    %% %% Smaller subset for testing
    %% {Time, Reply} = timer:tc(ct_netconfc, get, 
    %% 			     [nc1, [{'ManagedElement',[],
    %% 				     [{managedElementId, ["1"]},
    %% 				      {'SystemFunctions',[],
    %% 				       [{systemFunctionsId, ["1"]},
    %% 					{'Fm',[],
    %% 					 [{fmId, ["1"]}]}]}]}]]),
    ok = ct_netconfc:close_session(nc1),
    ct:pal("Time is ~p~n",[Time/1000000]),
    {ok, Data} = Reply,
    ets:new(moCount, [set, public, named_table]),
    ets:insert(moCount, {mo, 0}),
    ets:insert(moCount, {attributes, 0}),
    count_mo(hd(Data)),
    Sorted = lists:keysort(2, ets:tab2list(moCount)),
    ct:pal("Result~n~p~n", [lists:reverse(Sorted)]),
    [{mo, Mo}] = ets:lookup(moCount, mo),
    [{attributes, Attrs}] = ets:lookup(moCount, attributes),
    ct:pal("Performance~n~p MO/s~n~p s per attribute~nExecution time:~p~n",
	   [Mo/Time*1000000, Time/1000000/Attrs, Time/1000000]),
    ok.

count_mo({Id, _, Content}) ->
    case hd(atom_to_list(Id)) of
	C when C >= $A, C =< $Z ->
	    ets:update_counter(moCount, mo, 1),
	    case ets:lookup(moCount, Id) of
		[] ->
		    ets:insert(moCount, {Id, 1});
		[_] ->
		    ets:update_counter(moCount, Id, 1)
	    end;
	_ ->
	    ets:update_counter(moCount, attributes, 1)
    end,
    [count_mo(X)||X<-Content],
    ok;
count_mo(_) ->
    ok.


%%%--------------------------------------------------------------------
%%% LIBRARY FUNCTIONS
%%%--------------------------------------------------------------------


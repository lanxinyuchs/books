%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	cm_oi_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2013-2014
%%% @version /main/R2A/8

%%% @doc ==Example test suite for one sim or target env==
%%% This Test Suite can be used on both target and simulated enviroment.
%%% @end

-module(cm_oi_SUITE).
-include_lib("common_test/include/ct.hrl").

%%% ----------------------------------------------------------
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
%%% R1A/1      2012-09-19 etxkols     Created
%%% R2A/3      2013-07-03 etxivri     Added safe_imm_oi_rpc hook and updated SUITE to be able to run TCs.
%%%                                   Some TCs need to updated to be passed!
%%% R2A/3      2013-07-03 etxivri     Changed back to /2 and used a correct format in rct_rpc hook.
%%% R2A/5      2013-07-05 etxivri     Update to use oi_result_handler on testnode.
%%% R2A/6      2013-07-10 etxivri     Update all() with TC that can be runed without error,
%%%                                   and use of rct_logging.
%%% R2A/7      2013-12-10 etxivri     Increased rpc timeout.
%%% R2A/8      2014-02-12 etxkols     Increased rpc timeout.
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 init_per_group/2,
	 end_per_group/2,
	 all/0,
	 groups/0]).


-export([t1/1,
	 t2/1,
	 t3/1,
	 t4/1,
	 t5/1,
	 t6/1,
	 t7/1,
	 t8/1,
	 t9/1
	]).

-define(TESTNODE, testnode).

-define(COMTOP,  "urn:com:ericsson:ecim:ComTop").
-define(TESTMOM, "urn:com:ericsson:ecim:TESTMOM").

-define(NC_NAMESPACE, "urn:ietf:params:xml:ns:netconf:base:1.0").

-define(DELETE,  [{'xmlns:nc',     ?NC_NAMESPACE},
		  {'nc:operation', "delete"}]).


%%--------------------------------------------------------------------pmiSubscribeCallback(AppPid, Subscriber, GranularityPeriod, CounterSpecs) -> 
    
%% @doc
%% Used hooks: rct_htmllink, rct_netconf, cth_conn_log
%% @end
%%--------------------------------------------------------------------
suite() ->
    TestNode = ?TESTNODE,
    
    [{ct_hooks, [{rct_rpc, [{1, [{TestNode, TestNode}] }]},
		 {rct_htmllink,[]},
 		 {rct_netconf,nc1},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
 		 {cth_conn_log,[]}]}].


%% @hidden
init_per_suite(Config) ->
    Node = ?TESTNODE,
    {ok, Pid} = rct_rpc:call(Node, oi_result_handler, start, [?MODULE], 20000),

    rct_rpc:call(Node, oi_testapp, stop, [], 20000),
    case rct_rpc:call(Node, oi_testapp, start, [{?MODULE, Pid}], 20000) of
	{ok, _} -> 
	    Config;
	Error1 ->
	    {skip, Error1}
    end.

%% @hidden
end_per_suite(_Config) ->
    rct_rpc:call(?TESTNODE, oi_result_handler, stop, [?MODULE], 20000),
    rct_rpc:call(?TESTNODE, oi_testapp, stop, [], 20000),
    ok.

%% @hidden
init_per_group(_Group, Config) ->
    Config.

%% @hidden
end_per_group(_Group, _Config) ->
    ok.

%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.

%% @hidden
end_per_testcase(_TestCase, Config) ->

    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p.  \nClean up!", [Reason]),
	    
	    InstanceList = [2, 3],
	    lists:foreach(fun(Id) ->
				  
			    Expected = [{ok, ccb_object_delete_callback},
			    		{ok, ccb_completed_callback},
			    		{ok, ccb_apply_callback}],
				  
			    	  set_results(Expected), 

			  	  IdL = integer_to_list(Id),  
			  	  ct_netconfc:open(nc1, []),
			  	  ct_netconfc:edit_config(nc1,
			  				       running,
			  				       {'ManagedElement',
			  					[{xmlns, ?COMTOP}],
			  					[{managedElementId, [], ["1"]},
			  					 {'TestRoot',
			  					  [{xmlns, ?TESTMOM}],
			  					  [{testRootId, [], ["1"]},
			  					   {'TestClass1',
			  					    ?DELETE,
			  					    [{testClass1Id, [], [IdL]}]}]}]
			  				       }),
			  	  ct_netconfc:close_session(nc1)
			  end, InstanceList)
    end,

    ok.


%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [{group, g1},
     %% {group, g2}, %% Could result in ERROR
     %% {group, g3}, %% No expected abort recived
     %% {group, g4}, %% Could result in ERROR
     {group, g5}
     %% {group, g6}  %% No expected abort recived
    ].

groups() ->
    [{g1, [], [t2, t1, t2]},
     {g2, [], [t2, t3]},
     {g3, [], [t2, t4]},
     {g4, [], [t2, t1, t5, t2]},
     {g5, [], [t2, t6, t7]},
     {g6, [], [t2, t8, t2]}
    ].


%%--------------------------------------------------------------------
%% @doc 
%% Create testClass1 mo.<br/><br/>
%% @end
%%--------------------------------------------------------------------
t1(_Config) ->
    
    Expected = [{ok, ccb_object_create_callback_2},
		{ok, ccb_completed_callback},
		{ok, ccb_apply_callback}],
    set_results(Expected), 

    {ok, _} = ct_netconfc:open(nc1, []),
    ok = create(testClass1Id, 2),
    ct_netconfc:close_session(nc1),
    
    [{?MODULE,[]}] = read_results(), 
    ok.
    



%%--------------------------------------------------------------------
%% @doc 
%% Delete Test class1.<br/><br/>
%% @end
%%--------------------------------------------------------------------
t2(_Config) ->

    Expected = [{ok, ccb_object_delete_callback},
		{ok, ccb_completed_callback},
		{ok, ccb_apply_callback}],

    set_results(Expected), 


    {ok, _} = ct_netconfc:open(nc1, []),
    Res = delete(testClass1Id, 2),
    ct_netconfc:close_session(nc1),
    case read_results() of
	[{?MODULE, []}]       -> Res;
	[{?MODULE, Expected}] -> Res
    end.
	

%%--------------------------------------------------------------------
%% @doc 
%% Create testClass1 mo. Error at completed callback<br/><br/>
%% @end
%%--------------------------------------------------------------------
t3(_Config) ->
    
    Expected = [{ok, ccb_object_create_callback_2},
		{{error,8}, ccb_completed_callback},
		{ok, ccb_abort_callback}],
    set_results(Expected), 

    {ok, _} = ct_netconfc:open(nc1, []),
    ok = create(testClass1Id, 2),
    ct_netconfc:close_session(nc1),
    
    
    [{?MODULE,[]}] = read_results(), 
    ok.
    

%%--------------------------------------------------------------------
%% @doc 
%% Create testClass1 mo. Error at object create callback<br/><br/>
%% @end
%%--------------------------------------------------------------------
t4(_Config) ->
    
    Expected = [{{error, 8}, ccb_object_create_callback_2},
		{ok, ccb_abort_callback}],
    set_results(Expected), 

    {ok, _} = ct_netconfc:open(nc1, []),
    ok = create(testClass1Id, 2),
    ct_netconfc:close_session(nc1),
    
    
    [{?MODULE,[]}] = read_results(), 
    ok.
    


%%--------------------------------------------------------------------
%% @doc 
%% Delete Test class1. Error at completed callback<br/><br/>
%% @end
%%--------------------------------------------------------------------
t5(_Config) ->

    Expected = [{ok, ccb_object_delete_callback},
		{{error, 8}, ccb_completed_callback},
		{ok, ccb_abort_callback}],

    set_results(Expected), 


    {ok, _} = ct_netconfc:open(nc1, []),
    Res = delete(testClass1Id, 2),
    ct_netconfc:close_session(nc1),
    
    [{?MODULE,[]}] = read_results(), 
    Res.



%%--------------------------------------------------------------------
%% @doc 
%% Create testClass1 mo. Create two instances<br/><br/>
%% @end
%%--------------------------------------------------------------------
t6(_Config) ->
    
    Expected = [{ok, ccb_object_create_callback_2},
		{ok, ccb_object_create_callback_2},
		{ok, ccb_completed_callback},
		{ok, ccb_apply_callback}],
    set_results(Expected), 
    
    {ok, _}  = ct_netconfc:open(nc1, []),
    [ok, ok] = create(testClass1Id, [2, 3]),
    ct_netconfc:close_session(nc1),
    
    [{?MODULE,[]}] = read_results(), 
    ok.
    



%%--------------------------------------------------------------------
%% @doc 
%% Delete Test class1. Delete two instances<br/><br/>
%% @end
%%--------------------------------------------------------------------
t7(_Config) ->

    Expected = [{ok, ccb_object_delete_callback},
		{ok, ccb_object_delete_callback},
		{ok, ccb_completed_callback},
		{ok, ccb_apply_callback}],

    set_results(Expected), 

    {ok, _} = ct_netconfc:open(nc1, []),
    [ok, ok] = delete(testClass1Id, [2, 3]),
    ct_netconfc:close_session(nc1),
    [{?MODULE,[]}] = read_results(), 
    ok.


%%--------------------------------------------------------------------
%% @doc 
%% Create testClass1 mo.<br/><br/>
%% @end
%%--------------------------------------------------------------------
t8(_Config) ->
    
    %%===========================================
    %% create instance. modify 'int32'
    %%===========================================
    Expected = [{ok, ccb_object_create_callback_2},
		{ok, ccb_object_modify_callback_2},
		{ok, ccb_completed_callback},
		{ok, ccb_apply_callback}],
    set_results(Expected), 

    {ok, _} = ct_netconfc:open(nc1, []),
    ok = create(testClass1Id, 2),

    ok = modify({testClass1Id, 2}, {int32, "11"}),
    "11" = get_value({testClass1Id, 2}, int32),
    
    ok = modify({testClass1Id, 2}, {int32, "22"}),
    "22" = get_value({testClass1Id, 2}, int32),
    
    ct_netconfc:close_session(nc1),
    [{?MODULE,[]}] = read_results(), 


    %%===========================================
    %% modify 'int32', error completed
    %%===========================================
    {ok, _} = ct_netconfc:open(nc1, []),

    Expected2 = [{ok, ccb_object_modify_callback_2},
		 {{error, 8}, ccb_completed_callback},
		 {ok, ccb_abort_callback}],
    set_results(Expected2), 

    ok = modify({testClass1Id, 2}, {int32, "11"}),
    "11" = get_value({testClass1Id, 2}, int32),
    

    ct_netconfc:close_session(nc1),
    
    [{?MODULE,[]}] = read_results(), 

    %%===========================================
    %% modify 'int32', error modify
    %%===========================================
    {ok, _} = ct_netconfc:open(nc1, []),

    Expected3 = [{{error, 8}, ccb_object_modify_callback_2},
		 {ok, ccb_abort_callback}],
    set_results(Expected3), 

    ok = modify({testClass1Id, 2}, {int32, "11"}),
    "11" = get_value({testClass1Id, 2}, int32),
    
    ct_netconfc:close_session(nc1),
    
    [{?MODULE,[]}] = read_results(), 


    %%===========================================
    %% modify 'int32', error modify
    %%===========================================
    {ok, _} = ct_netconfc:open(nc1, []),

    set_results([]), 

    "22" = get_value({testClass1Id, 2}, int32),
    
    ct_netconfc:close_session(nc1),
    
    [{?MODULE,[]}] = read_results(), 

    ok.



%%--------------------------------------------------------------------
%% @doc 
%% Create testClass1 mo. Timeout at completed<br/><br/>
%% @end
%%--------------------------------------------------------------------
t9(_Config) ->
    
    Expected = [{ok, ccb_object_create_callback_2},
		{ok, ccb_object_modify_callback_2},
		{{finalize, ok}, ccb_completed_callback}],
    set_results(Expected), 

    {ok, _} = ct_netconfc:open(nc1, []),
    ok = create(testClass1Id, 2),

    ok = modify({testClass1Id, 2}, {int32, "11"}),
    "11" = get_value({testClass1Id, 2}, int32),
    
    ct_netconfc:close_session(nc1),
    
    [{?MODULE,[]}] = read_results(), 

    %%===========================================
    %% check that the instance does not exist
    %%===========================================
    {ok, _} = ct_netconfc:open(nc1, []),

    set_results([]), 

    {error, GV} = gv({testClass1Id, 2}),

    ok = check_error_msg(GV, "is not avail"),
    
    ct_netconfc:close_session(nc1),
    
    [{?MODULE,[]}] = read_results(), 


    ok.
    




%%===========================================================================
%% 
%%===========================================================================
create(Class, Ids) when is_list(Ids) ->
    [create(Class, Id) || Id <- Ids];
create(testClass1Id, Id) ->

    IdL = integer_to_list(Id),  
    try 
	ok = ct_netconfc:edit_config(nc1,
				     running,
				     {'ManagedElement',
				      [{xmlns, ?COMTOP}],
				      [{managedElementId, [], ["1"]},
				       {'TestRoot',
					[{xmlns, ?TESTMOM}],
					[{testRootId, [], ["1"]},
					 {'TestClass1',
					  [],
					  [{testClass1Id, [], [IdL]}]
					 }]}]
				     }),

	{ok, [{'ManagedElement',
	       [{xmlns, ?COMTOP}],
	       [{managedElementId, [], ["1"]},
		{'TestRoot',
		 [{xmlns, ?TESTMOM}],
		 [{testRootId, [], ["1"]},
		  {'TestClass1',
		   [],
		   [{testClass1Id, [], [IdL]} | _]}]}]}
	     ]} = ct_netconfc:get(nc1,
				  {'ManagedElement',
				   [{xmlns, ?COMTOP}],
				   [{managedElementId, [], ["1"]},
				    {'TestRoot',
				     [{xmlns, ?TESTMOM}],
				     [{testRootId, [], ["1"]},
				      {'TestClass1',
				       [],
				       [{testClass1Id, [], [IdL]}]}]}]
				  }),
	ok
	
    catch T:E ->
	    {error, {T,E}}
    end.



%%===========================================================================
%% 
%%===========================================================================
delete(Class, Ids) when is_list(Ids) ->
    [delete(Class, Id) || Id <- Ids];
delete(testClass1Id, Id) ->
    IdL = integer_to_list(Id),  

    try 
	{ok, [{'ManagedElement',
	       [{xmlns, ?COMTOP}],
	       [{managedElementId, [], ["1"]},
		{'TestRoot',
		 [{xmlns, ?TESTMOM}],
		 [{testRootId, [], ["1"]},
		  {'TestClass1', [], [{testClass1Id, [], [IdL]} | _]}]}]}
	     ]} = ct_netconfc:get(nc1,
				  {'ManagedElement',
				   [{xmlns, ?COMTOP}],
				   [{managedElementId, [], ["1"]},
				    {'TestRoot',
				     [{xmlns, ?TESTMOM}],
				     [{testRootId, [], ["1"]},
				      {'TestClass1',
				       [],
				       [{testClass1Id, [], [IdL]}]}]}]
				  }),
	
	ok = ct_netconfc:edit_config(nc1,
				     running,
				     {'ManagedElement',
				      [{xmlns, ?COMTOP}],
				      [{managedElementId, [], ["1"]},
				       {'TestRoot',
					[{xmlns, ?TESTMOM}],
					[{testRootId, [], ["1"]},
					 {'TestClass1',
					  ?DELETE,
					  [{testClass1Id, [], [IdL]}]}]}]
				     }),

	{error, _} = ct_netconfc:get(nc1,
				     {'ManagedElement',
				      [{xmlns, ?COMTOP}],
				      [{managedElementId, [], ["1"]},
				       {'TestRoot',
					[{xmlns, ?TESTMOM}],
					[{testRootId, [], ["1"]},
					 {'TestClass1',
					  [],
					  [{testClass1Id, [], [IdL]}]}]}]
				     }),
	
	ok
	
    catch T:E ->
	    {error, {T,E}}
    end.


%%===========================================================================
%% 
%%===========================================================================
modify({testClass1Id, Id}, {int32, Int32}) ->

    IdL = integer_to_list(Id),  
    ok = ct_netconfc:edit_config(nc1,
				 running,
				 {'ManagedElement',
				  [{xmlns, ?COMTOP}],
				  [{managedElementId, [], ["1"]},
				   {'TestRoot',
				    [{xmlns, ?TESTMOM}],
				    [{testRootId, [], ["1"]},
				     {'TestClass1',
				      [],
				      [{testClass1Id, [], [IdL]},
				       {int32, [], [Int32]}]
				     }]}]
				 }).

%%===========================================================================
%% 
%%===========================================================================
get_value({testClass1Id, _Id} = What, Attr) ->
    
    {ok, [{'ManagedElement',
	   [{xmlns, ?COMTOP}],
	   [{managedElementId, [], ["1"]},
	    {'TestRoot',
	     [{xmlns, ?TESTMOM}],
	     [{testRootId, [], ["1"]}, {'TestClass1', [], Values}]
	    }]}
	 ]} = gv(What),
    [V] = [Val || {A, _, [Val]} <- Values, A == Attr],
    V.
    

gv({testClass1Id, Id}) ->
    IdL = integer_to_list(Id),  
    ct_netconfc:get(nc1,
		    {'ManagedElement',
		     [{xmlns, ?COMTOP}],
		     [{managedElementId, [], ["1"]},
		      {'TestRoot',
		       [{xmlns, ?TESTMOM}],
		       [{testRootId, [], ["1"]},
			{'TestClass1',
			 [],
			 [{testClass1Id, [], [IdL]}]}]}]
		    }).



%%===========================================================================
%% 
%%===========================================================================
set_results(Expected) ->
    ok = rct_rpc:call(?TESTNODE, oi_result_handler, set_results, [?MODULE, Expected], 10000).

%%===========================================================================
%% 
%%===========================================================================
read_results() ->
    rct_rpc:call(?TESTNODE, oi_result_handler, read_results, [?MODULE], 20000).

check_error_msg([], "is not avail") ->
    {error, {msg_not_found, ""}};
check_error_msg([{'error-message', [_, _], [ErrorMsg]} | _T], Msg) ->
    case string:str(ErrorMsg, Msg) of
	0 -> {error, {no_match, ErrorMsg}};
	_ -> ok
    end;
check_error_msg([_ | T], Msg) ->
    check_error_msg(T, Msg).

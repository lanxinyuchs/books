%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	cm_basic_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R7A/R10A/R11A/3

%%% @doc ==Example test suite for one sim or target env==
%%% This Test Suite can be used on both target and simulated enviroment.
%%% @end

-module(cm_basic_SUITE).
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
%%% R1A/2      2013-06-17 etxjovp     Add hook rct_logging
%%% R1A/4      2013-10-22 etxkols     Removed compiler warning
%%%                                   cm_basic_SUITE.erl:500: Warning: variable
%%%                                   'Filter' is unused
%%% R2A/5      2013-10-30 etxpeno     Test cases for bi-directional associations
%%% R2A/6      2013-12-27 etxberb     Changed order of creation (create
%%%                                   TestClass9 first) in test_bidir_createMos.
%%% R2A/7      2014-01-17 etxpeno     Adapt to COM 3.4
%%% R3A/1      2015-07-13 etxjovp     Add group definitions used by CS CI
%%% R4A/1      2015-08-25 etxkols     Adaption to new testmom
%%% R5A/1      2016-02-29 etxpeno     add action related test cases
%%% R7A/1      2016-10-06 etxpeno     changes due to correction of TR HV31163
%%% R11A/3     2017-09-12 etxivri     Removed test_parent_child_relation_in_parent_fragment from all
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


-export([test_root_mo/1,
	 test_class1_all/1,
	 test_class1_1_create/1,
	 test_class1_1_delete/1,
	 test_bidir_createMos/1,
	 test_bidir_get1/1,
	 test_bidir_changeAttr1/1,
	 test_bidir_get2/1,
	 test_bidir_deleteMo1/1,
	 test_bidir_get3/1,
	 test_bidir_deleteMo2/1,
	 test_bidir_get4/1,
	 test_bidir_deleteMo3/1,
	 test_bidir_deleteMo4/1,
	 test_bidir_deleteAttr1/1,
	 test_bidir_deleteAttr2/1,
	 test_bidir_changeAttr_cardinality/1,
	 test_bidir_deleteAllMos/1,
	 test_actions/1,
	 test_actions_fail1/1,
	 test_actions_fail2/1,
	 test_actions_fail3/1,
	 test_actions_fail4/1,
         test_parent_child_relation_in_parent_fragment/1]).

-include_lib("safe/include/safe.hrl").
-include_lib("safe/include/safe_imm.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_netconf, cth_conn_log
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{ct_hooks, [{rct_htmllink,[]},
		 {rct_netconf,nc1},
		 {cth_conn_log,[]},
                 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
					       ["sa_ais_err_bad_operation"]}}]}},
                 {rct_safe_imm_oi_rpc,[{finalize_on_fail,true}]}
		]}].


%% @hidden
init_per_suite(Config) ->
    Config.
%% @hidden
end_per_suite(_Config) ->
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
end_per_testcase(_TestCase, _Config) ->
    ct_netconfc:close_session(nc1),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [test_root_mo,
     {group, testclass1},
     {group, test_bidir},
     test_actions,
     test_actions_fail1,
     test_actions_fail2,
     test_actions_fail3,
     test_actions_fail4
     %% test_parent_child_relation_in_parent_fragment
    ].

groups() ->
    AllGroup=all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},
     {sdc__qual__all__1__group, [], []},
     {testclass1, [], [test_class1_1_create,
		       test_class1_1_delete]},
     {test_bidir, [], [test_bidir_createMos,
		       test_bidir_get1,
		       test_bidir_changeAttr1,
		       test_bidir_get2,
		       test_bidir_deleteMo1,
		       test_bidir_get3,
		       test_bidir_deleteMo2,
		       test_bidir_get4,
		       test_bidir_deleteMo3,
		       test_bidir_createMos,
		       test_bidir_get1,
		       test_bidir_deleteMo4,
		       test_bidir_get1,
		       test_bidir_deleteAttr1,
		       test_bidir_get3,
		       test_bidir_deleteAttr2,
		       test_bidir_get4,
		       %% test_bidir_changeAttr_cardinality,
		       test_bidir_deleteAllMos]}].


%%--------------------------------------------------------------------
%% @doc
%% Test root mo.<br/><br/>
%% @end
%%--------------------------------------------------------------------
test_root_mo(_Config) ->
    {ok, _} = ct_netconfc:open(nc1, []),

    {ok, [{'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],["1"]},
	    {'TestRoot',
	     [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	     [{testRootId,[],["1"]}|Rest]}]}]} =
	ct_netconfc:get(nc1,{'ManagedElement',
			     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			     [{managedElementId,[],["1"]},
			      {'TestRoot',
			       [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
			       [{testRootId,[],["1"]}]}]}),

    case Rest of
	[] ->
	    ct:fail("test_root_mo Rest = []");
	_ ->
	    ok
    end,

    ok = ct_netconfc:close_session(nc1).

%%--------------------------------------------------------------------
%% @doc
%% Test class1.<br/><br/>
%% @end
%%--------------------------------------------------------------------
test_class1_all(_Config) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    ok = ct_netconfc:edit_config(nc1,running,{'ManagedElement',
					      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
					      [{managedElementId,[],["1"]},
					       {'TestRoot',
						[{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
						[{testRootId,[],["1"]},
						 {'TestClass1',
						  [],
						  [{testClass1Id,[],["2"]}]}]}]}),
    {ok, [{'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],["1"]},
	    {'TestRoot',
	     [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	     [{testRootId,[],["1"]},
	      {'TestClass1',
	       [],
	       [{testClass1Id,[],["2"]}|_]}]}]}]} =
	ct_netconfc:get(nc1,
			{'ManagedElement',
			 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			 [{managedElementId,[],["1"]},
			  {'TestRoot',
			   [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
			   [{testRootId,[],["1"]},
			    {'TestClass1',
			     [],
			     [{testClass1Id,[],["2"]}]}]}]}),

    ok = ct_netconfc:edit_config(nc1,
				 running,
				 {'ManagedElement',
				  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				  [{managedElementId,[],["1"]},
				   {'TestRoot',
				    [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
				    [{testRootId,[],["1"]},
				     {'TestClass1',
				      [{'xmlns:nc',
					"urn:ietf:params:xml:ns:netconf:base:1.0"},
				       {'nc:operation',"delete"}],
				      [{testClass1Id,[],["2"]}]}]}]}),
    {error, _} = ct_netconfc:get(nc1,
				 {'ManagedElement',
				  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				  [{managedElementId,[],["1"]},
				   {'TestRoot',
				    [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
				    [{testRootId,[],["1"]},
				     {'TestClass1',
				      [],
				      [{testClass1Id,[],["2"]}]}]}]}),

    ok = ct_netconfc:close_session(nc1).

test_class1_1_create(_Config) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    ok = ct_netconfc:edit_config(nc1,running,
				 {'ManagedElement',
				  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				  [{managedElementId,[],["1"]},
				   {'TestRoot',
				    [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
				    [{testRootId,[],["1"]},
				     {'TestClass1',
				      [],
				      [{testClass1Id,[],["2"]}]}]}]}),
    {ok, [{'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],["1"]},
	    {'TestRoot',
	     [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	     [{testRootId,[],["1"]},
	      {'TestClass1',
	       [],
	       [{testClass1Id,[],["2"]}|_]}]}]}]} =
	ct_netconfc:get(nc1,
			{'ManagedElement',
			 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			 [{managedElementId,[],["1"]},
			  {'TestRoot',
			   [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
			   [{testRootId,[],["1"]},
			    {'TestClass1',
			     [],
			     [{testClass1Id,[],["2"]}]}]}]}),

    ok = ct_netconfc:close_session(nc1).



test_class1_1_delete(_Config) ->
    {ok, _} = ct_netconfc:open(nc1, []),

    {ok, [{'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],["1"]},
	    {'TestRoot',
	     [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	     [{testRootId,[],["1"]},
	      {'TestClass1',
	       [],
	       [{testClass1Id,[],["2"]}|_]}]}]}]} =
	ct_netconfc:get(nc1,
			{'ManagedElement',
			 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			 [{managedElementId,[],["1"]},
			  {'TestRoot',
			   [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
			   [{testRootId,[],["1"]},
			    {'TestClass1',
			     [],
			     [{testClass1Id,[],["2"]}]}]}]}),

    ok = ct_netconfc:edit_config(nc1,
				 running,
				 {'ManagedElement',
				  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				  [{managedElementId,[],["1"]},
				   {'TestRoot',
				    [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
				    [{testRootId,[],["1"]},
				     {'TestClass1',
				      [{'xmlns:nc',
					"urn:ietf:params:xml:ns:netconf:base:1.0"},
				       {'nc:operation',"delete"}],
				      [{testClass1Id,[],["2"]}]}]}]}),
    {error, _} = ct_netconfc:get(nc1,
				 {'ManagedElement',
				  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				  [{managedElementId,[],["1"]},
				   {'TestRoot',
				    [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
				    [{testRootId,[],["1"]},
				     {'TestClass1',
				      [],
				      [{testClass1Id,[],["2"]}]}]}]}),

    ok = ct_netconfc:close_session(nc1).

test_bidir_createMos(_Config) ->
    %% * Create ManagedElement=1,TestRoot=1,TestClass8=1 and set uses to
    %% "ManagedElement=1,TestRoot=1,TestClass9=1",
    %% "ManagedElement=1,TestRoot=1,TestClass9=2"

    %% * Create ManagedElement=1,TestRoot=1,TestClass8=2 and set uses to
    %% "ManagedElement=1,TestRoot=1,TestClass9=1"

    %% * Create ManagedElement=1,TestRoot=1,TestClass9=1

    %% * Create ManagedElement=1,TestRoot=1,TestClass9=2

    %% * Create ManagedElement=1,TestRoot=1,TestClass9=3

    {ok, _} = ct_netconfc:open(nc1, []),

    EditConfig =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],["1"]},
	  {'TestRoot',
	   [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	   [{testRootId,[],["1"]},
	    {'TestClass8',[],
	     [{testClass8Id,[],["1"]},
	      {uses,[],["ManagedElement=1,TestRoot=1,TestClass9=1"]},
	      {uses,[],["ManagedElement=1,TestRoot=1,TestClass9=2"]}
	     ]
	    },
	    {'TestClass8',[],
	     [{testClass8Id,[],["2"]},
	      {uses,[],["ManagedElement=1,TestRoot=1,TestClass9=1"]}
	     ]
	    },
	    {'TestClass9',[],[{testClass9Id,[],["1"]}]},
	    {'TestClass9',[],[{testClass9Id,[],["2"]}]},
	    {'TestClass9',[],[{testClass9Id,[],["3"]}]}
	   ]
	  }]},

    ok = ct_netconfc:edit_config(nc1, running, EditConfig),

    ok = ct_netconfc:close_session(nc1).

test_bidir_get1(_Config) ->
    %% * Read ManagedElement=1,TestRoot=1,TestClass8=1
    %% * Read ManagedElement=1,TestRoot=1,TestClass8=2
    %% * Read ManagedElement=1,TestRoot=1,TestClass9=1
    %% Check reservedBy
    %% Should be "ManagedElement=1,TestRoot=1,TestClass8=1",
    %%           "ManagedElement=1,TestRoot=1,TestClass8=2"

    %% * Read ManagedElement=1,TestRoot=1,TestClass9=2
    %% Check reservedBy
    %% Should be "ManagedElement=1,TestRoot=1,TestClass8=1"

    %% * Read ManagedElement=1,TestRoot=1,TestClass9=3
    %% Should be empty

    {ok, _} = ct_netconfc:open(nc1, []),

    Filter = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],["1"]},
	       {'TestRoot',
		[{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
		[{testRootId,[],["1"]},
		 {'TestClass8',[],[{testClass8Id,[],["1"]}]},
		 {'TestClass8',[],[{testClass8Id,[],["2"]}]},
		 {'TestClass9',[],[{testClass9Id,[],["1"]}]},
		 {'TestClass9',[],[{testClass9Id,[],["2"]}]},
		 {'TestClass9',[],[{testClass9Id,[],["3"]}]}
		]
	       }]},

    {ok,
     [{'ManagedElement',
       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
       [{managedElementId,[],["1"]},
	{'TestRoot',
	 [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	 [{testRootId,[],["1"]},
	  {'TestClass8',[],
	   [{uses,[],["ManagedElement=1,TestRoot=1,TestClass9=1"]},
	    {uses,[],["ManagedElement=1,TestRoot=1,TestClass9=2"]},
	    {testClass8Id,[],["1"]}]},
	  {'TestClass8',[],
	   [{uses,[],["ManagedElement=1,TestRoot=1,TestClass9=1"]},
	    {testClass8Id,[],["2"]}]},
	  {'TestClass9',[],
	   [{reservedBy,[],["ManagedElement=1,TestRoot=1,TestClass8=1"]},
	    {reservedBy,[],["ManagedElement=1,TestRoot=1,TestClass8=2"]},
	    {testClass9Id,[],["1"]}]},
	  {'TestClass9',[],
	   [{reservedBy,[],["ManagedElement=1,TestRoot=1,TestClass8=1"]},
	    {testClass9Id,[],["2"]}]},
	  {'TestClass9',[],[{testClass9Id,[],["3"]}]}]}]}]} = ct_netconfc:get(nc1, Filter),

    ok = ct_netconfc:close_session(nc1).

test_bidir_changeAttr1(_Config) ->
    %% * Update ManagedElement=1,TestRoot=1,TestClass8=1: set uses to
    %% "ManagedElement=1,TestRoot=1,TestClass9=3"
    %% "ManagedElement=1,TestRoot=1,TestClass9=1",

    {ok, _} = ct_netconfc:open(nc1, []),

    EditConfig =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],["1"]},
	  {'TestRoot',
	   [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	   [{testRootId,[],["1"]},
	    {'TestClass8',[],
	     [{testClass8Id,[],["1"]},
	      {uses,[],["ManagedElement=1,TestRoot=1,TestClass9=3"]},
	      {uses,[],["ManagedElement=1,TestRoot=1,TestClass9=1"]}
	     ]
	    }
	   ]
	  }]},

    ok = ct_netconfc:edit_config(nc1, running, EditConfig),

    ok = ct_netconfc:close_session(nc1).

test_bidir_get2(_Config) ->
    %% * Read ManagedElement=1,TestRoot=1,TestClass9=1
    %% Check reservedBy
    %% Should be "ManagedElement=1,TestRoot=1,TestClass8=1",
    %%           "ManagedElement=1,TestRoot=1,TestClass8=2"

    %% * Read ManagedElement=1,TestRoot=1,TestClass9=2
    %% Should be empty

    %% * Read ManagedElement=1,TestRoot=1,TestClass9=3
    %% Check reservedBy
    %% Should be "ManagedElement=1,TestRoot=1,TestClass8=1"

    {ok, _} = ct_netconfc:open(nc1, []),

    Filter = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],["1"]},
	       {'TestRoot',
		[{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
		[{testRootId,[],["1"]},
		 {'TestClass9',[],[{testClass9Id,[],["1"]}]},
		 {'TestClass9',[],[{testClass9Id,[],["2"]}]},
		 {'TestClass9',[],[{testClass9Id,[],["3"]}]}
		]
	       }
	      ]
	     },

    {ok,
     [{'ManagedElement',
       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
       [{managedElementId,[],["1"]},
	{'TestRoot',
	 [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	 [{testRootId,[],["1"]},
	  {'TestClass9',[],
	   [{reservedBy,[],["ManagedElement=1,TestRoot=1,TestClass8=1"]},
	    {reservedBy,[],["ManagedElement=1,TestRoot=1,TestClass8=2"]},
	    {testClass9Id,[],["1"]}]},
	  {'TestClass9',[],[{testClass9Id,[],["2"]}]},
	  {'TestClass9',[],
	   [{reservedBy,[],["ManagedElement=1,TestRoot=1,TestClass8=1"]},
	    {testClass9Id,[],["3"]}]}]}]}]} = ct_netconfc:get(nc1, Filter),
    ok = ct_netconfc:close_session(nc1).

test_bidir_deleteMo1(_Config) ->
    %% * Delete ManagedElement=1,TestRoot=1,TestClass8=1

    {ok, _} = ct_netconfc:open(nc1, []),

    EditConfig = {'ManagedElement',
		  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		  [{managedElementId,[],["1"]},
		   {'TestRoot',
		    [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
		    [{testRootId,[],["1"]},
		     {'TestClass8',
		      [{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
		       {'nc:operation',"delete"}],
		      [{testClass8Id,[],["1"]}]}
		    ]
		   }]},

    ok = ct_netconfc:edit_config(nc1, running, EditConfig),

    ok = ct_netconfc:close_session(nc1).

test_bidir_get3(_Config) ->
    %% * Read ManagedElement=1,TestRoot=1,TestClass9=1
    %% Check reservedBy
    %% Should be "ManagedElement=1,TestRoot=1,TestClass8=2"

    %% * Read ManagedElement=1,TestRoot=1,TestClass9=2
    %% Should be empty

    %% * Read ManagedElement=1,TestRoot=1,TestClass9=3
    %% Should be empty

    {ok, _} = ct_netconfc:open(nc1, []),

    Filter = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],["1"]},
	       {'TestRoot',
		[{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
		[{testRootId,[],["1"]},
		 {'TestClass9',[],[{testClass9Id,[],["1"]}]},
		 {'TestClass9',[],[{testClass9Id,[],["2"]}]},
		 {'TestClass9',[],[{testClass9Id,[],["3"]}]}
		]
	       }
	      ]
	     },

    {ok, [{'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],["1"]},
	    {'TestRoot',
	     [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	     [{testRootId,[],["1"]},
	      {'TestClass9',[],
	       [{reservedBy,[],["ManagedElement=1,TestRoot=1,TestClass8=2"]},
		{testClass9Id,[],["1"]}]},
	      {'TestClass9',[],[{testClass9Id,[],["2"]}]},
	      {'TestClass9',[],[{testClass9Id,[],["3"]}]}]}]}]} = ct_netconfc:get(nc1, Filter),

    ok = ct_netconfc:close_session(nc1).

test_bidir_deleteMo2(_Config) ->
    %% * Delete ManagedElement=1,TestRoot=1,TestClass8=2

    {ok, _} = ct_netconfc:open(nc1, []),

    EditConfig = {'ManagedElement',
		  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		  [{managedElementId,[],["1"]},
		   {'TestRoot',
		    [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
		    [{testRootId,[],["1"]},
		     {'TestClass8',
		      [{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
		       {'nc:operation',"delete"}],
		      [{testClass8Id,[],["2"]}]}
		    ]
		   }]},

    ok = ct_netconfc:edit_config(nc1, running, EditConfig),

    ok = ct_netconfc:close_session(nc1).

test_bidir_get4(_Config) ->
    %% * Read ManagedElement=1,TestRoot=1,TestClass9=1
    %% Should be empty

    %% * Read ManagedElement=1,TestRoot=1,TestClass9=2
    %% Should be empty

    %% * Read ManagedElement=1,TestRoot=1,TestClass9=3
    %% Should be empty

    {ok, _} = ct_netconfc:open(nc1, []),

    Filter = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],["1"]},
	       {'TestRoot',
		[{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
		[{testRootId,[],["1"]},
		 {'TestClass9',[],[{testClass9Id,[],["1"]}]},
		 {'TestClass9',[],[{testClass9Id,[],["2"]}]},
		 {'TestClass9',[],[{testClass9Id,[],["3"]}]}
		]
	       }
	      ]
	     },


    {ok, [{'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],["1"]},
	    {'TestRoot',
	     [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	     [{testRootId,[],["1"]},
	      {'TestClass9',[],[{testClass9Id,[],["1"]}]},
	      {'TestClass9',[],[{testClass9Id,[],["2"]}]},
	      {'TestClass9',[],[{testClass9Id,[],["3"]}]}
	     ]
	    }
	   ]
	  }
	 ]} = ct_netconfc:get(nc1, Filter),

    ok = ct_netconfc:close_session(nc1).

test_bidir_deleteMo3(_Config) ->
    %% * Delete ManagedElement=1,TestRoot=1,TestClass9=1

    {ok, _} = ct_netconfc:open(nc1, []),

    EditConfig = {'ManagedElement',
		  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		  [{managedElementId,[],["1"]},
		   {'TestRoot',
		    [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
		    [{testRootId,[],["1"]},
		     {'TestClass9',
		      [{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
		       {'nc:operation',"delete"}],
		      [{testClass9Id,[],["1"]}]}
		    ]
		   }
		  ]
		 },

    ok = ct_netconfc:edit_config(nc1, running, EditConfig),

    ok = ct_netconfc:close_session(nc1).

test_bidir_deleteMo4(_Config) ->
    %% * Delete ManagedElement=1,TestRoot=1,TestClass9=1
    %% It should fail at close_session

    {ok, _} = ct_netconfc:open(nc1, []),

    EditConfig = {'ManagedElement',
		  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		  [{managedElementId,[],["1"]},
		   {'TestRoot',
		    [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
		    [{testRootId,[],["1"]},
		     {'TestClass9',
		      [{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
		       {'nc:operation',"delete"}],
		      [{testClass9Id,[],["1"]}]}
		    ]
		   }
		  ]
		 },

    ok = ct_netconfc:edit_config(nc1, running, EditConfig),

    {error, _Reason} = ct_netconfc:close_session(nc1).

test_bidir_deleteAttr1(_Config) ->
    %% * Delete ManagedElement=1,TestRoot=1,TestClass8=1,uses

    {ok, _} = ct_netconfc:open(nc1, []),

    EditConfig =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],["1"]},
	  {'TestRoot',
	   [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	   [{testRootId,[],["1"]},
	    {'TestClass8',
	     [],
	     [{testClass8Id,[],["1"]},
	      {uses,
	       [{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
		{'nc:operation',"delete"}], []}]}
	   ]
	  }]},

    ok = ct_netconfc:edit_config(nc1, running, EditConfig),

    ok = ct_netconfc:close_session(nc1).

test_bidir_deleteAttr2(_Config) ->
    %% * Delete ManagedElement=1,TestRoot=1,TestClass8=2,uses

    {ok, _} = ct_netconfc:open(nc1, []),

    EditConfig =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],["1"]},
	  {'TestRoot',
	   [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	   [{testRootId,[],["1"]},
	    {'TestClass8',
	     [],
	     [{testClass8Id,[],["2"]},
	      {uses,
	       [{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
		{'nc:operation',"delete"}], []}]}
	   ]
	  }]},

    ok = ct_netconfc:edit_config(nc1, running, EditConfig),

    ok = ct_netconfc:close_session(nc1).

test_bidir_changeAttr_cardinality(_Config) ->
    %% * Update ManagedElement=1,TestRoot=1,TestClass8=1: set uses to
    %% "ManagedElement=1,TestRoot=1,TestClass9=1",
    %% "ManagedElement=1,TestRoot=1,TestClass9=2"
    %% "ManagedElement=1,TestRoot=1,TestClass9=3"
    %% This should fail since it is too many server MOs (2 are maximum)

    {ok, _} = ct_netconfc:open(nc1, []),

    EditConfig =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],["1"]},
	  {'TestRoot',
	   [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	   [{testRootId,[],["1"]},
	    {'TestClass8',[],
	     [{testClass8Id,[],["1"]},
	      {uses,[],["ManagedElement=1,TestRoot=1,TestClass9=1"]},
	      {uses,[],["ManagedElement=1,TestRoot=1,TestClass9=2"]},
	      {uses,[],["ManagedElement=1,TestRoot=1,TestClass9=3"]}
	     ]
	    }
	   ]
	  }]},

    ok = ct_netconfc:edit_config(nc1, running, EditConfig),

    %% Fail
    {error, _} = ct_netconfc:close_session(nc1).

test_bidir_deleteAllMos(_Config) ->
    %% * Delete ManagedElement=1,TestRoot=1,TestClass9=1

    %% * Delete ManagedElement=1,TestRoot=1,TestClass9=2

    %% * Delete ManagedElement=1,TestRoot=1,TestClass9=3

    %% * Delete ManagedElement=1,TestRoot=1,TestClass8=1

    %% * Delete ManagedElement=1,TestRoot=1,TestClass8=2

    {ok, _} = ct_netconfc:open(nc1, []),

    EditConfig =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],["1"]},
	  {'TestRoot',
	   [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	   [{testRootId,[],["1"]},
	    {'TestClass9',
	     [{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
	      {'nc:operation',"delete"}],
	     [{testClass9Id,[],["1"]}]},
	    {'TestClass9',
	     [{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
	      {'nc:operation',"delete"}],
	     [{testClass9Id,[],["2"]}]},
	    {'TestClass9',
	     [{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
	      {'nc:operation',"delete"}],
	     [{testClass9Id,[],["3"]}]},
	    {'TestClass8',
	     [{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
	      {'nc:operation',"delete"}],
	     [{testClass8Id,[],["1"]}]},
	    {'TestClass8',
	     [{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
	      {'nc:operation',"delete"}],
	     [{testClass8Id,[],["2"]}]}
	   ]
	  }]},

    ok = ct_netconfc:edit_config(nc1, running, EditConfig),

    ok = ct_netconfc:close_session(nc1).

test_actions(_Config) ->

    AdmOpCbFun =
	fun(#safe_imm_oi_admin_operation_callback_2{
	       imm_oi_handle = Handle,
	       invocation = Inv,
	       object_name = "testClass3Id=1,testClass2Id=1,TESTMOMtestRootId=1",
	       operation_id = 1,
	       params = [#safe_imm_admin_operation_params_2{param_name = "multipleValue",
							    param_type = ?SAFE_IMM_ATTR_INT32,
							    param_buffer = 0}]}) ->
		R = [#safe_imm_admin_operation_params_2{param_name = "result_x",
							param_type = ?SAFE_IMM_ATTR_INT32,
							param_buffer = 1},
		     #safe_imm_admin_operation_params_2{param_name = "result_y",
							param_type = ?SAFE_IMM_ATTR_STRING,
							param_buffer = "y"},
		     #safe_imm_admin_operation_params_2{param_name = "result_z_1",
							param_type = ?SAFE_IMM_ATTR_INT32,
							param_buffer = 1},
		     #safe_imm_admin_operation_params_2{param_name = "result_z_2",
							param_type = ?SAFE_IMM_ATTR_INT32,
							param_buffer = 2},
		     #safe_imm_admin_operation_params_2{param_name = "result_z_3",
							param_type = ?SAFE_IMM_ATTR_INT32,
							param_buffer = 3}],
		rct_safe_imm_oi_rpc:admin_operation_result_o2(Handle,
							      Inv,
							      ?SAFE_AIS_OK,
							      R),
		ok;
	   (#safe_imm_oi_admin_operation_callback_2{
	       imm_oi_handle = Handle,
	       invocation = Inv,
	       object_name = "testClass3Id=1,testClass2Id=1,TESTMOMtestRootId=1",
	       operation_id = 1,
	       params = [#safe_imm_admin_operation_params_2{param_name = "multipleValue",
							    param_type = ?SAFE_IMM_ATTR_INT32,
							    param_buffer = 1}]}) ->
		R = [#safe_imm_admin_operation_params_2{param_name = "result_1_x",
							param_type = ?SAFE_IMM_ATTR_INT32,
							param_buffer = 1},
		     #safe_imm_admin_operation_params_2{param_name = "result_1_y",
							param_type = ?SAFE_IMM_ATTR_STRING,
							param_buffer = "y"},
		     #safe_imm_admin_operation_params_2{param_name = "result_1_z_1",
							param_type = ?SAFE_IMM_ATTR_INT32,
							param_buffer = 1},
		     #safe_imm_admin_operation_params_2{param_name = "result_1_z_2",
							param_type = ?SAFE_IMM_ATTR_INT32,
							param_buffer = 2},
		     #safe_imm_admin_operation_params_2{param_name = "result_1_z_3",
							param_type = ?SAFE_IMM_ATTR_INT32,
							param_buffer = 3},
		     #safe_imm_admin_operation_params_2{param_name = "result_2_z_1",
							param_type = ?SAFE_IMM_ATTR_INT32,
							param_buffer = 1},
		     #safe_imm_admin_operation_params_2{param_name = "result_2_x",
							param_type = ?SAFE_IMM_ATTR_INT32,
							param_buffer = 1},
		     #safe_imm_admin_operation_params_2{param_name = "result_2_y",
							param_type = ?SAFE_IMM_ATTR_STRING,
							param_buffer = "y"},
		     #safe_imm_admin_operation_params_2{param_name = "result_2_z_2",
							param_type = ?SAFE_IMM_ATTR_INT32,
							param_buffer = 2},
		     #safe_imm_admin_operation_params_2{param_name = "result_2_z_3",
							param_type = ?SAFE_IMM_ATTR_INT32,
							param_buffer = 3}],
		rct_safe_imm_oi_rpc:admin_operation_result_o2(Handle,
							      Inv,
							      ?SAFE_AIS_OK,
							      R),
		ok;
	   (#safe_imm_oi_admin_operation_callback_2{
	       imm_oi_handle = Handle,
	       invocation = Inv,
	       object_name = "testClass3Id=1,testClass2Id=1,TESTMOMtestRootId=1",
	       operation_id = 2,
	       params = [#safe_imm_admin_operation_params_2{param_name = "multipleValue",
							    param_type = ?SAFE_IMM_ATTR_INT32,
							    param_buffer = 0}]}) ->
		R = [#safe_imm_admin_operation_params_2{param_name = "result",
							param_type = ?SAFE_IMM_ATTR_INT32,
							param_buffer = 1}],
		rct_safe_imm_oi_rpc:admin_operation_result_o2(Handle,
							      Inv,
							      ?SAFE_AIS_OK,
							      R),
		ok;
	   (#safe_imm_oi_admin_operation_callback_2{
	       imm_oi_handle = Handle,
	       invocation = Inv,
	       object_name = "testClass3Id=1,testClass2Id=1,TESTMOMtestRootId=1",
	       operation_id = 2,
	       params = [#safe_imm_admin_operation_params_2{param_name = "multipleValue",
							    param_type = ?SAFE_IMM_ATTR_INT32,
							    param_buffer = 1}]}) ->
		R = [#safe_imm_admin_operation_params_2{param_name = "result_1",
							param_type = ?SAFE_IMM_ATTR_INT32,
							param_buffer = 1},
		     #safe_imm_admin_operation_params_2{param_name = "result_2",
							param_type = ?SAFE_IMM_ATTR_INT32,
							param_buffer = 2}],
		rct_safe_imm_oi_rpc:admin_operation_result_o2(Handle,
							      Inv,
							      ?SAFE_AIS_OK,
							      R),
		ok;
	   (#safe_imm_oi_admin_operation_callback_2{
	       imm_oi_handle = Handle,
	       invocation = Inv,
	       object_name = "testClass3Id=1,testClass2Id=1,TESTMOMtestRootId=1",
	       operation_id = 7,
	       params = [#safe_imm_admin_operation_params_2{param_name = "test1Action",
							    param_type = ?SAFE_IMM_ATTR_STRING,
							    param_buffer = String}]}) ->
		%% test a param_name not set to a value in the spec
		R = [#safe_imm_admin_operation_params_2{param_name = "",
							param_type = ?SAFE_IMM_ATTR_STRING,
							param_buffer = String}],
		rct_safe_imm_oi_rpc:admin_operation_result_o2(Handle,
							      Inv,
							      ?SAFE_AIS_OK,
							      R),
		ok;
	   (#safe_imm_oi_admin_operation_callback_2{
	       imm_oi_handle = Handle,
	       invocation = Inv,
	       object_name = "testClass3Id=1,testClass2Id=1,TESTMOMtestRootId=1",
	       operation_id = 8,
	       params = []}) ->
		R = [],
		rct_safe_imm_oi_rpc:admin_operation_result_o2(Handle,
							      Inv,
							      ?SAFE_AIS_OK,
							      R),
		ok;

	   (A) ->
		ct:pal("unknown callback: ~p", [A]),
		ok
	end,

    CbFun = #safe_imm_oi_callbacks_2{admin_operation = AdmOpCbFun},

    ImplName = "ImplementerOne",
    Class = "TESTMOMTestClass3",

    Handle = create_class_impl(CbFun, ImplName, Class),

    try
	{ok, _} = ct_netconfc:open(nc1, []),

	Action1 = {'ManagedElement',
		   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		   [{managedElementId,[],["1"]},
		    {'TestRoot',
		     [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
		     [{testRootId,[],["1"]},
		      {'TestClass2',
		       [{testClass2Id,[],["1"]},
			{'TestClass3', [{testClass3Id,[],["1"]},
					{test1,[],[{test1Action, [], ["String"]}]}]}
		       ]}
		     ]}
		   ]},
	R1 = [{'ManagedElement',
	       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	       [{managedElementId,[],["1"]},
		{'TestRoot',
		 [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
		 [{testRootId,[],["1"]},
		  {'TestClass2',[],
		   [{testClass2Id,[],["1"]},
		    {'TestClass3',[],
		     [{testClass3Id,[],["1"]},
		      {test1,[],
		       [{returnValue,[],["String"]}]}
		     ]}
		   ]}
		 ]}
	       ]}],
	{ok, R1} = ct_netconfc:action(nc1, Action1),

	Action2 = {'ManagedElement',
		   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		   [{managedElementId,[],["1"]},
		    {'TestRoot',
		     [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
		     [{testRootId,[],["1"]},
		      {'TestClass2',
		       [{testClass2Id,[],["1"]},
			{'TestClass3', [{testClass3Id,[],["1"]}, {test2,[],[]}]}
		       ]}
		     ]}
		   ]},
	ok = ct_netconfc:action(nc1, Action2),

	ActionSimpleSingle =
	    {'ManagedElement',
	     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	     [{managedElementId,[],["1"]},
	      {'TestRoot',
	       [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	       [{testRootId,[],["1"]},
		{'TestClass2',
		 [{testClass2Id,[],["1"]},
		  {'TestClass3', [{testClass3Id,[],["1"]},
				  {testSimpleType,[],[{multipleValue, [], ["false"]}]}]}
		 ]}
	       ]}
	     ]},
	RetSimpleSingle =
	    [{'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],["1"]},
	       {'TestRoot',
		[{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
		[{testRootId,[],["1"]},
		 {'TestClass2',[],
		  [{testClass2Id,[],["1"]},
		   {'TestClass3',[],
		    [{testClass3Id,[],["1"]},
		     {testSimpleType,[],[{returnValue,[],["1"]}]}
		    ]}
		  ]}
		]}
	      ]}],
	{ok, RetSimpleSingle} = ct_netconfc:action(nc1, ActionSimpleSingle),

	ActionSimpleMulti =
	    {'ManagedElement',
	     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	     [{managedElementId,[],["1"]},
	      {'TestRoot',
	       [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	       [{testRootId,[],["1"]},
		{'TestClass2',
		 [{testClass2Id,[],["1"]},
		  {'TestClass3', [{testClass3Id,[],["1"]},
				  {testSimpleType,[],[{multipleValue, [], ["true"]}]}]}
		 ]}
	       ]}
	     ]},
	RetSimpleMulti =
	    [{'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],["1"]},
	       {'TestRoot',
		[{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
		[{testRootId,[],["1"]},
		 {'TestClass2',[],
		  [{testClass2Id,[],["1"]},
		   {'TestClass3',[],
		    [{testClass3Id,[],["1"]},
		     {testSimpleType,[],[{returnValue,[],["1"]},
					 {returnValue,[],["2"]}]}
		    ]}
		  ]}
		]}
	      ]}],
	{ok, RetSimpleMulti} = ct_netconfc:action(nc1, ActionSimpleMulti),

	ActionComplexSimple =
	    {'ManagedElement',
	     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	     [{managedElementId,[],["1"]},
	      {'TestRoot',
	       [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	       [{testRootId,[],["1"]},
		{'TestClass2',
		 [{testClass2Id,[],["1"]},
		  {'TestClass3', [{testClass3Id,[],["1"]},
				  {testComplexType,[],[{multipleValue, [], ["false"]}]}]}
		 ]}
	       ]}
	     ]},
	RetComplexSimple =
	    [{'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],["1"]},
	       {'TestRoot',
		[{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
		[{testRootId,[],["1"]},
		 {'TestClass2',[],
		  [{testClass2Id,[],["1"]},
		   {'TestClass3',[],
		    [{testClass3Id,[],["1"]},
		     {testComplexType,[],
		      [{returnValue,
			[{struct,"Struct4"}],
			[{x,[],["1"]},{y,[],["y"]},
			 {z,[],["1"]},{z,[],["2"]},{z,[],["3"]}]}
		      ]}
		    ]}
		  ]}
		]}
	      ]}],
	{ok, RetComplexSimple} = ct_netconfc:action(nc1, ActionComplexSimple),

	ActionComplexMulti =
	    {'ManagedElement',
	     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	     [{managedElementId,[],["1"]},
	      {'TestRoot',
	       [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	       [{testRootId,[],["1"]},
		{'TestClass2',
		 [{testClass2Id,[],["1"]},
		  {'TestClass3', [{testClass3Id,[],["1"]},
				  {testComplexType,[],[{multipleValue, [], ["true"]}]}]}
		 ]}
	       ]}
	     ]},
	RetComplexMulti =
	    [{'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],["1"]},
	       {'TestRoot',
		[{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
		[{testRootId,[],["1"]},
		 {'TestClass2',[],
		  [{testClass2Id,[],["1"]},
		   {'TestClass3',[],
		    [{testClass3Id,[],["1"]},
		     {testComplexType,[],
		      [{returnValue,
			[{struct,"Struct4"}],
			[{x,[],["1"]},{y,[],["y"]},
			 {z,[],["1"]},{z,[],["2"]},{z,[],["3"]}]},
		       {returnValue,
			[{struct,"Struct4"}],
			[{x,[],["1"]},{y,[],["y"]},
			 {z,[],["1"]},{z,[],["2"]},{z,[],["3"]}]}
		      ]}
		    ]}
		  ]}
		]}
	      ]}],
	{ok, RetComplexMulti} = ct_netconfc:action(nc1, ActionComplexMulti),

	ok = ct_netconfc:close_session(nc1),
        delete_class_impl(Class, ImplName, Handle)
    catch
        _:Reason ->
            ct:pal("TC fail, Reason: ~p", [Reason]),
            cleanup_class_impl(Class, ImplName, Handle),
            ct:fail(Reason)
    end.

test_actions_fail1(_Config) ->

    AdmOpCbFun =
	fun(#safe_imm_oi_admin_operation_callback_2{imm_oi_handle = Handle,
						    invocation    = Inv}) ->
		R = [#safe_imm_admin_operation_params_2{param_name = "SaImmAdminOperationError",
							param_type = ?SAFE_IMM_ATTR_STRING,
							param_buffer = "@ComNbi@This is an error"}],
		rct_safe_imm_oi_rpc:admin_operation_result_o2(Handle,
							      Inv,
							      ?SAFE_AIS_ERR_BAD_OPERATION,
							      R),
		ok
	end,

    CbFun = #safe_imm_oi_callbacks_2{admin_operation = AdmOpCbFun},

    ImplName = "ImplementerOne",
    Class = "TESTMOMTestClass3",

    Handle = create_class_impl(CbFun, ImplName, Class),

    try
	{ok, _} = ct_netconfc:open(nc1, []),

	ActionErrString =
	    {'ManagedElement',
	     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	     [{managedElementId,[],["1"]},
	      {'TestRoot',
	       [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	       [{testRootId,[],["1"]},
		{'TestClass2',
		 [{testClass2Id,[],["1"]},
		  {'TestClass3', [{testClass3Id,[],["1"]},
				  {testSimpleType,[],[]}]}
		 ]}
	       ]}
	     ]},
	{error, [{'error-type',[_],[_]},
		 {'error-tag',[_],["operation-failed"]},
		 {'error-severity',[_],["error"]},
		 {'error-message',_,
		  ["Request could not be performed - resource not available, [This is an error]"]}]} = ct_netconfc:action(nc1, ActionErrString),

        delete_class_impl(Class, ImplName, Handle)
    catch
        _:Reason ->
            ct:pal("TC fail, Reason: ~p", [Reason]),
            cleanup_class_impl(Class, ImplName, Handle),
            ct:fail(Reason)
    end.

test_actions_fail2(_Config) ->

    AdmOpCbFun =
	fun(#safe_imm_oi_admin_operation_callback_2{imm_oi_handle = Handle,
						    invocation    = Inv}) ->
		R = [],
		rct_safe_imm_oi_rpc:admin_operation_result_o2(Handle,
							      Inv,
							      ?SAFE_AIS_ERR_BAD_OPERATION,
							      R),
		ok
	end,

    CbFun = #safe_imm_oi_callbacks_2{admin_operation = AdmOpCbFun},

    ImplName = "ImplementerOne",
    Class = "TESTMOMTestClass3",

    Handle = create_class_impl(CbFun, ImplName, Class),

    try
	{ok, _} = ct_netconfc:open(nc1, []),

	ActionErrString =
	    {'ManagedElement',
	     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	     [{managedElementId,[],["1"]},
	      {'TestRoot',
	       [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	       [{testRootId,[],["1"]},
		{'TestClass2',
		 [{testClass2Id,[],["1"]},
		  {'TestClass3', [{testClass3Id,[],["1"]},
				  {testSimpleType,[],[]}
				 ]}
		 ]}
	       ]}
	     ]},
	{error, [{'error-type',[_],[_]},
		 {'error-tag',[_],["operation-failed"]},
		 {'error-severity',[_],["error"]},
		 {'error-message',_,
		  ["Request could not be performed - resource not available, [The requested operation is not allowed. (sa_ais_err_bad_operation)\n]"]}]} = ct_netconfc:action(nc1, ActionErrString),

	delete_class_impl(Class, ImplName, Handle)
    catch
	_:Reason ->
	    ct:pal("TC fail, Reason: ~p", [Reason]),
	    cleanup_class_impl(Class, ImplName, Handle),
	    ct:fail(Reason)
    end.

test_actions_fail3(_Config) ->

    AdmOpCbFun =
	fun(#safe_imm_oi_admin_operation_callback_2{imm_oi_handle = Handle,
						    invocation    = Inv}) ->
		R = [#safe_imm_admin_operation_params_2{param_name = "SaImmAdminOperationError",
							param_type = ?SAFE_IMM_ATTR_INT32,
							param_buffer = 0},
		     #safe_imm_admin_operation_params_2{param_name = "SaImmAdminOperationError",
							param_type = ?SAFE_IMM_ATTR_STRING,
							param_buffer = "test"}],
		rct_safe_imm_oi_rpc:admin_operation_result_o2(Handle,
							      Inv,
							      ?SAFE_AIS_ERR_BAD_OPERATION,
							      R),
		ok
	end,

    CbFun = #safe_imm_oi_callbacks_2{admin_operation = AdmOpCbFun},

    ImplName = "ImplementerOne",
    Class = "TESTMOMTestClass3",

    Handle = create_class_impl(CbFun, ImplName, Class),

    try
	{ok, _} = ct_netconfc:open(nc1, []),

	ActionErrString =
	    {'ManagedElement',
	     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	     [{managedElementId,[],["1"]},
	      {'TestRoot',
	       [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	       [{testRootId,[],["1"]},
		{'TestClass2',
		 [{testClass2Id,[],["1"]},
		  {'TestClass3', [{testClass3Id,[],["1"]},
				  {testSimpleType,[],[]}
				 ]}
		 ]}
	       ]}
	     ]},
	{error, [{'error-type',[_],[_]},
		 {'error-tag',[_],["operation-failed"]},
		 {'error-severity',[_],["error"]},
		 {'error-message',_,
		  ["Request could not be performed - resource not available, [The requested operation is not allowed. (sa_ais_err_bad_operation)\n]"]}]} = ct_netconfc:action(nc1, ActionErrString),

	delete_class_impl(Class, ImplName, Handle)
    catch
	_:Reason ->
	    ct:pal("TC fail, Reason: ~p", [Reason]),
	    cleanup_class_impl(Class, ImplName, Handle),
	    ct:fail(Reason)
    end.

test_actions_fail4(_Config) ->
    {ok, _} = ct_netconfc:open(nc1, []),

    ActionErrString =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],["1"]},
	  {'TestRoot',
	   [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	   [{testRootId,[],["1"]},
	    {'TestClass2',
	     [{testClass2Id,[],["1"]},
	      {'TestClass3', [{testClass3Id,[],["1"]},
			      {testSimpleType,[],[]}
			     ]}
	     ]}
	   ]}
	 ]},
    {error, [{'error-type',[_],[_]},
	     {'error-tag',[_],["operation-failed"]},
	     {'error-severity',[_],["error"]},
	     {'error-message',_,
	      ["Request could not be performed - resource not available, [An entity, referenced by the invoker, does not exist. (sa_ais_err_not_exist)\n]"]}]} = ct_netconfc:action(nc1, ActionErrString),

    ok.

test_parent_child_relation_in_parent_fragment(_Config) ->

    % Create Child under both Parent1 and Parent2.
    % Parent1 and Child are in different fragment than Parent2.
    % The relation Parent2 to Child is defined in Parent2 fragment,
    % normally it's in the Child fragment

    {ok, _} = ct_netconfc:open(nc1, []),
    ok = ct_netconfc:edit_config(nc1,running,
				 {'ManagedElement',
				  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				  [{managedElementId,[],["1"]},
				   {'Parent1',
				    [{xmlns,"urn:com:ericsson:ecim:SP549_1"}],
				    [{parent1Id,[],["1"]},
				     {'Child',
				      [],
				      [{childId,[],["1"]}]}]},
				   {'Parent2',
				    [{xmlns,"urn:com:ericsson:ecim:SP549_2"}],
				    [{parent2Id,[],["1"]},
				     {'Child',
				      [{xmlns,"urn:com:ericsson:ecim:SP549_1"}],
				      [{childId,[],["1"]}]}]}]}),
    {ok, [{'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],["1"]},
	    {'Parent1',
	     [{xmlns,"urn:com:ericsson:ecim:SP549_1"}],
	     [{parent1Id,[],["1"]},
	      {'Child',
	       [],
	       [{childId,[],["1"]}|_]}]},
	    {'Parent2',
	     [{xmlns,"urn:com:ericsson:ecim:SP549_2"}],
	     [{parent2Id,[],["1"]},
	      {'Child',
	       [{xmlns,"urn:com:ericsson:ecim:SP549_1"}],
	       [{childId,[],["1"]}|_]}]}]}]} =
	ct_netconfc:get(nc1,
			{'ManagedElement',
			 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			 [{managedElementId,[],["1"]},
			  {'Parent1',
			   [{xmlns,"urn:com:ericsson:ecim:SP549_1"}],
			   [{parent1Id,[],["1"]},
			    {'Child',
			     [],
			     [{childId,[],["1"]}]}]},
			  {'Parent2',
			   [{xmlns,"urn:com:ericsson:ecim:SP549_2"}],
			   [{parent2Id,[],["1"]},
			    {'Child',
			     [{xmlns,"urn:com:ericsson:ecim:SP549_1"}],
			     [{childId,[],["1"]}]}]}]}),

    ok = ct_netconfc:close_session(nc1).


create_class_impl(CbFun, IMPL_NAME, IMM_TEST_CLASS) ->
    ct:pal("Initialize OI Handle", []),
    {ok, Handle, Vsn} = rct_safe_imm_oi_rpc:initialize_2(CbFun),
    ct:pal("Initialize ok: ~p ~p",[Handle, Vsn]),
    ct:pal("Set Implementer name: ~p", [IMPL_NAME]),
    ok = rct_safe_imm_oi_rpc:implementer_set(Handle, IMPL_NAME),
    ct:pal("Set Class Implementer for: ~p", [IMM_TEST_CLASS]),
    ok = rct_safe_imm_oi_rpc:class_implementer_set(Handle, IMM_TEST_CLASS),
    Handle.

delete_class_impl(IMM_TEST_CLASS, IMPL_NAME, Handle) ->
    ct:pal("Release Class Implementer for: ~p", [IMM_TEST_CLASS]),
    ok = rct_safe_imm_oi_rpc:class_implementer_release(Handle, IMM_TEST_CLASS),
    ct:pal("Clear Implementer ~p", [IMPL_NAME]),
    ok = rct_safe_imm_oi_rpc:implementer_clear(Handle),
    ct:pal("Finalize OI Handle ~p", [Handle]),
    ok = rct_safe_imm_oi_rpc:finalize(Handle).

cleanup_class_impl(IMM_TEST_CLASS, IMPL_NAME, Handle) ->
    ct:pal("Release Class Implementer for: ~p", [IMM_TEST_CLASS]),
    rct_safe_imm_oi_rpc:class_implementer_release(Handle, IMM_TEST_CLASS),
    ct:pal("Clear Implementer ~p", [IMPL_NAME]),
    rct_safe_imm_oi_rpc:implementer_clear(Handle),
    ct:pal("Finalize OI Handle ~p", [Handle]),
    rct_safe_imm_oi_rpc:finalize(Handle).

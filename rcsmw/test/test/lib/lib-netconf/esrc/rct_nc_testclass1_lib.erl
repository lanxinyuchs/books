%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_nc_testclass1_lib.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R3A/R4A/R8A/1
%%%
%%% @doc == support lib when using netconf on testClass1 in TestRoot model. ==
%%% <br/>
%%%
%%%
%%% @end

-module(rct_nc_testclass1_lib).
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
%%% R2A/1      2013-09-16 etxivri     Created
%%% R2A/3      2013-09-19 etxivri     Added more functions.
%%% R2A/4      2013-09-20 etxivri     Added nc_get_mo_instance_no_check.
%%% R2A/5      2013-10-03 etxivri     Update list of nr of mo inst and attr.
%%% R2A/6      2013-10-15 etxivri     Created nc_get_several_mo_inst_check_inst_and_attr.
%%% R2A/6      2013-10-18 etxivri     Changed a function name that was missleading.
%%% R2A/7      2013-10-22 etxivri     Changed in check of attributes.
%%% R2A/8      2013-11-12 etxivri     added nc_get_mo_instance_check_attr.
%%% R2A/9      2013-11-12 etxivri     Inreased timeout in
%%%                                   nc_get_several_mo_inst_check_inst_and_attr
%%%                                   Added nc_get_all_under_testroot,
%%% R2A/11      2014-03-27 etxivri    Increased timeout vaules.
%%% R2A/12      2014-03-28 etxivri    Increased more timeout vaules.
%%% R2A/13      2014-05-26 etxivri    Update due to new com behaviour.
%%%                                   Several instances could get in differnts
%%%                                   order. Check need to be updated.
%%% R3A/1       2015-03-20 etxivri    Update to handle more MOs.
%%% R3A/2       2015-03-30 etxivri    Increased timeouts.
%%% R3A/3       2015-03-31 etxivri    Minor update.
%%% R4A/1       2015-08-27 etxivri    Adaption to new testmom
%%% R4A/2       2015-08-27 etxivri    More adaption to new testmom
%%% R4A/3       2015-09-01 etxivri    More adaption to new testmom
%%% R8A/1       2016-12-20 etxpeno    Don't set attribute struct1
%%%                                   (no support in SAFE for "Struct as attribute")
%%% ----------------------------------------------------------
%%%

% -compile([export_all]).

-export([
	 nc_add_mo_instance/2,
	 nc_add_mo_instance_and_attr/3,
	 nc_add_mo_instance_and_attr_no_check/3,
	 nc_delete_mo_instance/2,
	 nc_delete_mo_instance_no_check/2,
	 nc_add_nr_of_mo_instance_and_attr/2,
	 nc_del_nr_of_mo_instance/2,
	 nc_get_all_mo_models/1,
	 nc_get_all_under_testroot/1,
	 nc_get_mo_instance_check/2,
	 nc_get_mo_instance_check_attr/3,
	 nc_get_mo_instance_no_check/2,
	 nc_get_several_mo_instance_check/2,
	 nc_get_several_mo_inst_check_inst_and_attr/2,
	 nc_get_attribute_check/3,
	 nc_check_mo_instance_created/2,
	 nc_check_mo_instance_deleted/2
	]).


%% ===========================================================================
%% @doc
%% Create a mo instance of testClass1. <br/>
%% NC_Session = atom() <br/>
%% InstName = string() <br/>
%% @spec nc_add_mo_instance(NC_session, InstName) -> ok
%% @end
%% ===========================================================================
nc_add_mo_instance(NC_session, InstName) ->
    ok = ct_netconfc:edit_config(NC_session,
				 running,{'ManagedElement',
					  [{xmlns,
					    "urn:com:ericsson:ecim:ComTop"}],
					  [{managedElementId,[],["1"]},
					   {'TestRoot',
					    [{xmlns,
					      "urn:com:ericsson:ecim:TESTMOM"}],
					    [{testRootId,[],["1"]},
					     {'TestClass1',
					      [],
					      [{'testClass1Id',[],[InstName]}
					      ]}]}]}, 30000),
    ok.

%% ===========================================================================
%% @doc
%% Create a mo instance and a attribute of testClass1. <br/>
%% NC_Session = atom() <br/>
%% InstName = string() <br/>
%% AttrName = list of integer() example. "12345" <br/>
%% @spec nc_add_mo_instance_and_attr(NC_session, InstName, AttrName) -> ok
%% @end
%% ===========================================================================
nc_add_mo_instance_and_attr(NC_session, InstName, AttrName) ->
    ok = ct_netconfc:edit_config(NC_session,
				 running,{'ManagedElement',
					  [{xmlns,
					    "urn:com:ericsson:ecim:ComTop"}],
					  [{managedElementId,[],["1"]},
					   {'TestRoot',
					    [{xmlns,
					      "urn:com:ericsson:ecim:TESTMOM"}],
					    [{testRootId,[],["1"]},
					     {'TestClass1',
					      [],
					      [{'testClass1Id',[],[InstName]},
					       {int32,[],[AttrName]}
					      ]}]}]}, 30000),
    ok.

%% ===========================================================================
%% @doc
%% Create a mo instance and a attribute of testClass1. No check on answer. <br/>
%% NC_Session = atom() <br/>
%% InstName = string() <br/>
%% AttrName = list of integer() ex. "12345" <br/>
%% @spec nc_add_mo_instance_and_attr_no_check(NC_session, InstName, AttrName) -> ok
%% @end
%% ===========================================================================
nc_add_mo_instance_and_attr_no_check(NC_session, InstName, AttrName) ->
    ct_netconfc:edit_config(NC_session,
			    running,{'ManagedElement',
				     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				     [{managedElementId,[],["1"]},
				      {'TestRoot',
				       [{xmlns,
					 "urn:com:ericsson:ecim:TESTMOM"}],
				       [{testRootId,[],["1"]},
					{'TestClass1',
					 [],
					 [{'testClass1Id',[],[InstName]},
					  {int32,[],[AttrName]}
					 ]}]}]}, 30000).

%% ===========================================================================
%% @doc
%% Delete a mo instance of testClass1. <br/>
%% NC_Session = atom() <br/>
%% InstName = string() <br/>
%% @spec nc_delete_mo_instance(NC_session, InstName) -> ok
%% @end
%% ===========================================================================
nc_delete_mo_instance(NC_session, InstName) ->
    ok = ct_netconfc:edit_config(NC_session,
				 running,
				 {'ManagedElement',
				  [{xmlns,
				    "urn:com:ericsson:ecim:ComTop"}],
				  [{managedElementId,[],["1"]},
				   {'TestRoot',
				    [{xmlns,
				      "urn:com:ericsson:ecim:TESTMOM"}],
				    [{testRootId,[],["1"]},
				     {'TestClass1',
				      [{'xmlns:nc',
					"urn:ietf:params:xml:ns:netconf:"
					"base:1.0"},
				       {'nc:operation',"delete"}],
				      [{'testClass1Id',[],[InstName]}]}]}]},
				 30000),
    ok.

%% ===========================================================================
%% @doc
%% Delete a mo instance of testClass1. <br/>
%% NC_Session = atom() <br/>
%% InstName = string() <br/>
%% @spec nc_delete_mo_instance_no_check(NC_session, InstName) -> ok
%% @end
%% ===========================================================================
nc_delete_mo_instance_no_check(NC_session, InstName) ->
    ct_netconfc:edit_config(NC_session,
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
				 [{'testClass1Id',[],[InstName]}]}]}]}, 7200000).


%% ===========================================================================
%% @doc
%% Add several mo instances and attributes of testclass1 in one commit. <br/>
%% NC_Session = atom() <br/>
%% NrOfMoInstList = list of intingers() <br/>
%% @spec nc_add_nr_of_mo_instance_and_attr(NC_session, NrOfMoInstList) -> ok
%% @end
%% ===========================================================================
nc_add_nr_of_mo_instance_and_attr(NC_session, NrOfMoInstList) ->
    %% Craeate a list of the MO instances that shall be created.
    MO_Instances =
	lists:map(
	  fun(Nr) ->
		  InstName = atom_to_list(NC_session)++"_"++integer_to_list(Nr),
		  AttrName = integer_to_list(Nr),
		  {'TestClass1',
		   [],
		   [{'testClass1Id',[],[InstName]},
		    {int32,[],[AttrName]}
		   ]}
	  end,
	  NrOfMoInstList),

    %%% Add list of MO inst to testRoot level.
    All_MO_List = lists:append([{testRootId,[],["1"]}], MO_Instances),
    ct:pal("### All_MO_List created.", []),
    %% ct:pal("### ~p", [All_MO_List]),

    %% create all instances.
    ok = ct_netconfc:edit_config(NC_session,running,
				 {'ManagedElement',
				  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				  [{managedElementId,[],["1"]},
				   {'TestRoot',
				    [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
				    All_MO_List
				   }]}, 7200000),
    ok.

%% ===========================================================================
%% @doc
%% Delete several mo instances of testclass1 in one commit. <br/>
%% NC_Session = atom() <br/>
%% NrOfMoInstList = list of integers(). <br/>
%% @spec nc_del_nr_of_mo_instance(NC_session, NrOfMoInstList) -> ok
%% @end
%% ===========================================================================
nc_del_nr_of_mo_instance(NC_session, NrOfMoInstList) ->
    MOInstancesDel =
	lists:map(
	  fun(Nr) ->
		  InstName = atom_to_list(NC_session)++"_"++integer_to_list(Nr),
		  {'TestClass1',
		   [{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
		    {'nc:operation',"delete"}],
		   [{'testClass1Id',[],[InstName]}]}
	  end,
	  NrOfMoInstList),

    All_MO_instDel = lists:append([{testRootId,[],["1"]}], MOInstancesDel),

    ok = ct_netconfc:edit_config(NC_session,running,
				 {'ManagedElement',
				  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				  [{managedElementId,[],["1"]},
				   {'TestRoot',
				    [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
				    All_MO_instDel
				   }]}, 7200000),
    ok.

%% ===========================================================================
%% @doc
%% Get all Mo models. <br/>
%% NC_Session = atom() <br/>
%% @spec nc_get_all_mo_models(NC_session) -> ok
%% @end
%% ===========================================================================
nc_get_all_mo_models(NC_Session) ->
    {ok,_} = ct_netconfc:get(NC_Session,
			     {'ManagedElement',
			      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			      [{managedElementId,[],["1"]}]}, 30000),
    ok.

%% ===========================================================================
%% @doc
%% Get everything under TestRoot. <br/>
%% NC_Session = atom() <br/>
%% @spec nc_get_all_under_testroot(NC_session) -> TestRootList
%% @end
%% ===========================================================================
nc_get_all_under_testroot(NC_Session) ->
    {ok, [{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    	   [{managedElementId,[],["1"]},
    	    {'TestRoot',[{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
    	     TestRootList}]}] } =
	ct_netconfc:get(NC_Session,
			{'ManagedElement',
			 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			 [{managedElementId,[],["1"]},
			    {'TestRoot',
			     [{xmlns,
			       "urn:com:ericsson:ecim:TESTMOM"}],
			     [{testRootId,[],["1"]}]}
			 ]}, 300000),
    TestRootList.

%% ===========================================================================
%% @doc
%% Get MO instance. Check the rceived answer is expected. <br/>
%% - Get MO one by one. <br/>
%% NC_Session = atom()
%% InstName = string()
%% @spec nc_get_mo_instance_check(NC_Session, InstName) -> ok
%% @end
%% ===========================================================================
nc_get_mo_instance_check(NC_Session, InstName) ->
    {ok, [{'ManagedElement',
    	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    	   [{managedElementId,[],["1"]},
    	    {'TestRoot',
    	     [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
    	     [{testRootId,[],["1"]},
    	      {'TestClass1', [],
    	       AttrData
    	      }]}]}]}
	= ct_netconfc:get(NC_Session,
			  {'ManagedElement',
			   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			   [{managedElementId,[],["1"]},
			    {'TestRoot',
			     [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
			     [{testRootId,[],["1"]},
			      {'TestClass1',
			       [],
			       [{'testClass1Id',[],[InstName]}]}]}]}, 300000),

    true = lists:member({testClass1Id,[],[InstName]}, AttrData),

    ok.

%% ===========================================================================
%% @doc
%% Get MO instance. Check the rceived answer is expected. <br/>
%% - Get MO one by one. <br/>
%% NC_Session = atom()
%% InstName = string()
%% AttrName = list of integer() ex. "12345" <br/>
%% @spec nc_get_mo_instance_check_attr(NC_Session, InstName, AttrName) -> ok
%% @end
%% ===========================================================================
nc_get_mo_instance_check_attr(NC_Session, InstName, AttrName) ->
    {ok,[{'ManagedElement',
	  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	  [{managedElementId,[],["1"]},
	   {'TestRoot',
	    [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	    [{testRootId,[],["1"]},
	     {'TestClass1',[],
	      AttrData
	     }]}]}]}
	= ct_netconfc:get(NC_Session,
			  {'ManagedElement',
			   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			   [{managedElementId,[],["1"]},
			    {'TestRoot',
			     [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
			     [{testRootId,[],["1"]},
			      {'TestClass1',
			       [],
			       [{'testClass1Id',[],[InstName]}]}]}]}, 30000),

    ct:log("AttrData: ~p",[AttrData]),

    %% %%Check expected attributes exist
    true = lists:member({testClass1Id,[],[InstName]}, AttrData),
    true = lists:member({int32,[],[AttrName]}, AttrData),

    ok.


%% ===========================================================================
%% @doc
%% Get MO instance. No check of the rceived answer. <br/>
%% - Get MO one by one. <br/>
%% NC_Session = atom()
%% InstName = string()
%% @spec nc_get_mo_instance_no_check(NC_Session, InstName) -> ok
%% @end
%% ===========================================================================
nc_get_mo_instance_no_check(NC_Session, InstName) ->
    ct_netconfc:get(NC_Session,
    		    {'ManagedElement',
    		     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    		     [{managedElementId,[],["1"]},
    		      {'TestRoot',
    		       [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
    		       [{testRootId,[],["1"]},
    			{'TestClass1',
    			 [],
    			 [{'testClass1Id',[],[InstName]}]}]}]}, 30000).


%% ===========================================================================
%% @doc
%% Get several MO instance in one get operation. Check rceived reply os expected. <br/>
%% - Get several MO in one get opearation. <br/>
%% NC_Session = atom()
%% NrOfMoInstList = list of integers().
%% @spec nc_get_several_mo_instance_check(NC_Session, InstName) -> ok
%% @end
%% ===========================================================================
nc_get_several_mo_instance_check(NC_session, NrOfMoInstList) ->
    All_Instances =
	lists:map(
	  fun(Nr) ->
		  InstName = atom_to_list(NC_session)++"_"++integer_to_list(Nr),
		  {'TestClass1',[],[{'testClass1Id',[],[InstName]}]}
	  end,
	  NrOfMoInstList),

    GetAll_Inst_List = lists:append([{testRootId,[],["1"]}], All_Instances),

    %% Res = [{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    %% 	    [{managedElementId,[],["1"]},
    %% 	     {'TestRoot',[{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
    %% 	      [{testRootId,[],["1"]}]++All_Instances}]}],

    %% {ok, Res} = ct_netconfc:get(NC_session,
    %% 				{'ManagedElement',
    %% 				 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    %% 				 [{managedElementId,[],["1"]},
    %% 				  {'TestRoot',
    %% 				   [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
    %% 				   GetAll_Inst_List
    %% 				  }]}, 60000),

    %% ok.

    {ok, Res} = ct_netconfc:get(NC_session,
				{'ManagedElement',
				 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				 [{managedElementId,[],["1"]},
				  {'TestRoot',
				   [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
				   GetAll_Inst_List
				  }]}, 60000),

    [{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
      [{managedElementId,[],["1"]},
       {'TestRoot',[{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	Get_All_Instances}]}] = Res,

    [{testRootId,[],["1"]} | GetAllInstances] = Get_All_Instances,

    ct:pal("GetAllInstances: ~p",[GetAllInstances]),

    GetAllInstances.


%% ===========================================================================
%% @doc
%% Get several MO instance in one get operation. <br/>
%% Check inst and attr in rceived reply is expected. <br/>
%% - Get several MO in one get opearation. <br/>
%% NC_Session = atom()
%% NrOfMoInstList = list of integers().
%% @spec
%% nc_get_several_mo_inst_check_inst_and_attr(NC_Session, NrOfMoInstList) -> ok
%% @end
%% ===========================================================================
nc_get_several_mo_inst_check_inst_and_attr(NC_session, NrOfMoInstList) ->
    All_Instances =
	lists:map(
	  fun(Nr) ->
		  InstName =
		      atom_to_list(NC_session)++"_"++integer_to_list(Nr),
		  {'TestClass1',[],[{'testClass1Id',[],[InstName]}]}
	  end,
	  NrOfMoInstList),

    GetAll_Inst_List = lists:append([{testRootId,[],["1"]}], All_Instances),
    ct:pal("### All_MO_List created.", []),

    {ok, [{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    	   [{managedElementId,[],["1"]},
    	    {'TestRoot',[{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
    	     TestRootList}]}] } =
 	ct_netconfc:get(NC_session,
			{'ManagedElement',
			 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			 [{managedElementId,[],["1"]},
			  {'TestRoot',
			   [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
			   GetAll_Inst_List
			  }]}, 7200000),
    [{testRootId,[],["1"]} | All_Instances_Attr] = TestRootList,
    All_Instances_Attr.

%% ===========================================================================
%% @doc
%% Get attribute in MO instance. Check the rceived answer is expected.<br/>
%% - Get MO one by one. <br/>
%% NC_Session = atom()
%% InstName = string()
%% AttrName = list of integer() ex. "12345" <br/>
%% @spec nc_get_attribute_check(NC_Session, InstName, AttrName) -> ok
%% @end
%% ===========================================================================
nc_get_attribute_check(NC_Session, InstName, AttrName) ->
    %% {ok,[{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    %% 	  [{managedElementId,[],["1"]},
    %% 	   {'TestRoot',[{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
    %% 	    [{testRootId,[],["1"]},
    %% 	     {'TestClass1',[],
    %% 	      [{struct1,[{struct,"Struct1"}],
    %% 		AttrStructList},
    %% 	       {int32,[],[AttrName]},
    %% 	       {testClass1Id,[],[InstName]}]
    %% 	     }]}]}]}
    {ok,[{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	  [{managedElementId,[],["1"]},
	   {'TestRoot',[{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
	    [{testRootId,[],["1"]},
	     {'TestClass1',[],
	      AttrData
	     }]}]}]}
	= ct_netconfc:get(NC_Session,{'ManagedElement',
				       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				       [{managedElementId,[],["1"]},
					{'TestRoot',
					 [{xmlns,
					   "urn:com:ericsson:ecim:TESTMOM"}],
					 [{testRootId,[],["1"]},
					  {'TestClass1',[],
					   [{'testClass1Id',[],
					     [InstName]},
					    {int32,[],[]}
					   ]}]}]}, 30000),
    ct:log("AttrData: ~p",[AttrData]),

    %% ct:pal("AttrStructList: ~p",[AttrStructList]),
    %% %%Check expected attributes exist
    true = lists:member({testClass1Id,[],[InstName]}, AttrData),
    true = lists:member({int32,[],[AttrName]}, AttrData),

    ok.

%% ===========================================================================
%% @doc
%% Check MO instance created. <br/>
%% - read MO one by one. <br/>
%% NC_Session = atom()
%% InstName = string()
%% @spec nc_check_mo_instance_created(NC_Session, InstName) -> ok
%% @end
%% ===========================================================================
nc_check_mo_instance_created(NC_Session, InstName) ->
    {ok, [{'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],["1"]},
    				  {'TestRoot',
    				   [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
    				   [{testRootId,[],["1"]},
    				    {'TestClass1',
    				     [],
    				     _}]}]}]}
	= ct_netconfc:get(NC_Session,
			  {'ManagedElement',
			   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			   [{managedElementId,[],["1"]},
			    {'TestRoot',
			     [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
			     [{testRootId,[],["1"]},
			      {'TestClass1',
			       [],
			       [{'testClass1Id',[],[InstName]}]}]}]}, 30000),

    ok.


%% ===========================================================================
%% @doc
%% Check MO instance deleted. <br/>
%% - read MO one by one. <br/>
%% NC_SessionName = atom()
%% InstName = string()
%% @spec nc_check_mo_instance_deleted(NC_Session, InstName) -> ok
%% @end
%% ===========================================================================
nc_check_mo_instance_deleted(NC_SessionName, InstName) ->
    {error, [_,
	     _,
	     _,
	     {'error-message', _,  [Res]}
	    ]}
	= ct_netconfc:get(NC_SessionName,
			  {'ManagedElement',
			   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			   [{managedElementId,[],["1"]},
			    {'TestRoot',
			     [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
			     [{testRootId,[],["1"]},
			      {'TestClass1',
			       [],
			       [{'testClass1Id',[],[InstName]}]}]}]}, 7200000),

    case re:run(Res, "MO: ManagedElement=1,TestRoot=1,TestClass1="++
		    InstName++" is not available [(]no instance[)].") of
	{match, _} ->
	    ok;
	nomatch ->
	    ct:fail("MO inst still exist")
    end,

    ok.

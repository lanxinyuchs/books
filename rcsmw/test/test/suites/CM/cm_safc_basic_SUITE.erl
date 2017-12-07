%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	cm_safc_basic_SUITE.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2012-2016
%%% @version /main/R2A/R3A/R4A/R7A/1

%%% @doc ==CM IMM test suite for a single simulated and target environment==
%%% @end

-module(cm_safc_basic_SUITE).
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
%%% R1A/1      2012-10-24 eolaand     Created
%%% R2A/2      2013-04-26 etxivri     Added a new TC and hooks.
%%% R2A/3      2013-07-02 etxivri     Added 2 new TC and cleanup if TC fails.
%%%                                   Moved a TC to /ROB.
%%% R2A/4      2013-08-14 etxivri     Update error TCs.
%%%                                   Added filter "sa_ais_err_bad_operation" on ERROR.
%%% R2A/5      2013-08-14 etxivri     Changed a ct:pal to ct:log.
%%% R2A/6      2013-09-10 etxivri     Updates due to renamed saf-rpc hooks.
%%% R2A/6      2014-04-10 etxivri     Added erl logging on oi_testap.
%%% R2A/9      2014-06-10 etxberb     Added verify_error_message in
%%%                                   test_class_2_1_create_err_2oi.
%%% R2A/11     2014-09-23 etxberb     * Adjusted for introduction of validate.
%%%                                   * Temporarily disabled 3 cases in all/0.
%%% R2A/12     2014-10-08 etxberb     Enabled the 3 cases again.
%%% R2A/14     2014-10-15 etxpeno     Disable test_class_2_1_create_err_2oi
%%% R4A/1      2015-05-12 etxarnu     Adapation to COM 5.1
%%% R4A/2      2015-06-17 etxarnu     Update due to changed behaviour.
%%% R4A/4      2015-07-13 etxjovp     Add group definitions used by CS CI
%%% R4A/5      2015-08-25 etxkols     Adapting to new testmom build
%%% R4A/6      2015-09-04 etxpeno     Update due to corrected behavior in SAF
%%% R7A/1      2016-10-12 etxpeno     Update due to struct as attribute introduction
%%% R7A/2      2016-10-17 egjknpa     add FAKE_VNFM for vrcs 
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
         groups/0,
	 all/0]).


-export([test_class1_create_mod_delete/1,
	 test_class1_create_err/1,
	 test_class_1_2_create_mod_delete_2oi/1,
	 test_class_1_2_create_err_2oi/1,
	 test_class_2_1_create_err_2oi/1
	]).


%-compile(export_all).

-define(IMPL_NAME_1, "ImplementerOne").
-define(IMPL_NAME_2, "ImplementerTwo").
-define(IMM_TEST_CLASS1, "TESTMOMTestClass1").
-define(IMM_TEST_CLASS2, "TESTMOMTestClass2").

-define(SAFE_AIS_ERR_BAD_OPERATION, 20).
-define(SAFE_AIS_ERR_TIMEOUT, 5).

-define(SAFE_CCB_CB_CREATE, safe_imm_oi_ccb_object_create_callback_2).
-define(SAFE_CCB_CB_MOD, safe_imm_oi_ccb_object_modify_callback_2).
-define(SAFE_CCB_CB_DEL, safe_imm_oi_ccb_object_delete_callback).
-define(SAFE_CCB_CB_COMP, safe_imm_oi_ccb_completed_callback).
-define(SAFE_CCB_CB_APP, safe_imm_oi_ccb_apply_callback).
-define(SAFE_CCB_CB_ABORT, safe_imm_oi_ccb_abort_callback).

-define('ManagedElement', 'ManagedElement').
-define(ELEMENT_ID_1, {managedElementId,[],["1"]}).
-define(TEST_ROOT, 'TestRoot').
-define(TEST_ROOT_ID_1, {testRootId,[],["1"]}).
-define(COM_TOP, [{xmlns,"urn:com:ericsson:ecim:ComTop"}]).
-define(TEST_MOM, [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}]).
-define(TEST_CLASS1, 'TestClass1').
-define(TEST_CLASS2, 'TestClass2').
-define(TEST_CLASS1_ID_1, {'testClass1Id',[],["1"]}).
-define(TEST_CLASS1_ID_2, {'testClass1Id',[],["2"]}).
-define(TEST_CLASS1_ID_2_MOD_EXP, {'testClass1Id',[],["2"]},{int32,[],["65"]}).


-define(TEST_CLASS2_ID_2,
	[{'testClass2Id',[],["2"]},
	 {'TestClass3',[],[{testClass3Id,[],["2"]}]}]).
-define(TEST_CLASS2_ID_2_MOD,
	{'testClass2Id',[],["2"]},
	{'struct2',[{struct,"Struct2"}],[{struct2mem1,[],["init"]},
					 {struct2key,[],["3"]} ]}).
-define(TEST_CLASS2_ID_2_GET,
	[{'testClass2Id',[],["2"]}]).
-define(TEST_CLASS2_ID_2_EXP,
	[{'testClass2Id',[],["2"]},
	 {'TestClass3',[],[{testClass3Id,[],["2"]},
			   {managedElement,[],["helvete"]}]}]).
-define(TEST_CLASS2_ID_2_MOD_EXP,
	{'testClass2Id',[],["2"]},
	{'struct2',[{struct,"Struct2"}],[{struct2mem1,[],["init"]},
					 {struct2key,[],["3"]}]},
	{'TestClass3',[],[{testClass3Id,[],["2"]},
			  {managedElement,[],["helvete"]}]}).


%% [{testRootId,[],["1"]},
%%                      {'TestClass1',[],
%%                          [{testClass1Id,[],["1"]},
%%                           {struct1,
%%                               [{struct,"Struct1"}],
%%                               [{struct1mem2,[],["init"]},
%%                                {struct1mem1,[],["1"]}]},
%%                           {int32,[],["13"]}]}]


%% -define(ROOT_MO, {?'ManagedElement',
%% 		  ?COM_TOP,
%% 		  [?ELEMENT_ID_1,
%% 		   {?TEST_ROOT,
%% 		    ?TEST_MOM,
%% 		    [?TEST_ROOT_ID_1]}]}).

%% -define(ROOT_MO_EXP, {?'ManagedElement',
%% 		      ?COM_TOP,
%% 		      [?ELEMENT_ID_1,
%% 		       {?TEST_ROOT,
%% 			?TEST_MOM,
%% 			[?TEST_ROOT_ID_1|_]}]}).

%% -define(TEST_CLASS1_1, {?'ManagedElement',
%% 			?COM_TOP,
%% 			[?ELEMENT_ID_1,
%% 			 {?TEST_ROOT,
%% 			  ?TEST_MOM,
%% 			  [?TEST_ROOT_ID_1,
%% 			   {?TEST_CLASS1,
%% 			    [],
%% 			    [?TEST_CLASS1_ID_1]}]}]}).

%% -define(TEST_CLASS1_1_EXP, {?'ManagedElement',
%% 			    ?COM_TOP,
%% 			    [?ELEMENT_ID_1,
%% 			     {?TEST_ROOT,
%% 			      ?TEST_MOM,
%% 			      [?TEST_ROOT_ID_1,
%% 			       {?TEST_CLASS1,
%% 				[],
%% 				[?TEST_CLASS1_ID_1|_]}]}]}).

-define(TEST_CLASS1_2, {?'ManagedElement',
			?COM_TOP,
			[?ELEMENT_ID_1,
			 {?TEST_ROOT,
			  ?TEST_MOM,
			  [?TEST_ROOT_ID_1,
			   {?TEST_CLASS1,
			    [],
			    [?TEST_CLASS1_ID_2]}]}]}).

-define(TEST_CLASS2_2, {?'ManagedElement',
			?COM_TOP,
			[?ELEMENT_ID_1,
			 {?TEST_ROOT,
			  ?TEST_MOM,
			  [?TEST_ROOT_ID_1,
			   {?TEST_CLASS2,
			    [],
			    ?TEST_CLASS2_ID_2}]}]}).

-define(TEST_CLASS2_2_GET, {?'ManagedElement',
			?COM_TOP,
			[?ELEMENT_ID_1,
			 {?TEST_ROOT,
			  ?TEST_MOM,
			  [?TEST_ROOT_ID_1,
			   {?TEST_CLASS2,
			    [],
			    ?TEST_CLASS2_ID_2_GET}]}]}).

-define(TEST_CLASS1_2_EXP, {?'ManagedElement',
			    ?COM_TOP,
			    [?ELEMENT_ID_1,
			     {?TEST_ROOT,
			      ?TEST_MOM,
			      [?TEST_ROOT_ID_1,
			       {?TEST_CLASS1,
				[],
				[?TEST_CLASS1_ID_2|_]}]}]}).

-define(TEST_CLASS2_2_EXP, {?'ManagedElement',
			    ?COM_TOP,
			    [?ELEMENT_ID_1,
			     {?TEST_ROOT,
			      ?TEST_MOM,
			      [?TEST_ROOT_ID_1,
			       {?TEST_CLASS2,
				[],
				?TEST_CLASS2_ID_2_EXP} ]}]}).

-define(TEST_CLASS1_2_MOD_EXP, {?'ManagedElement',
				?COM_TOP,
				[?ELEMENT_ID_1,
				 {?TEST_ROOT,
				  ?TEST_MOM,
				  [?TEST_ROOT_ID_1,
				   {?TEST_CLASS1,
				    [],
				    [?TEST_CLASS1_ID_2_MOD_EXP]}]}]}).

-define(TEST_CLASS2_2_MOD_EXP, {?'ManagedElement',
				?COM_TOP,
				[?ELEMENT_ID_1,
				 {?TEST_ROOT,
				  ?TEST_MOM,
				  [?TEST_ROOT_ID_1,
				   {?TEST_CLASS2,
				    [],
				    [?TEST_CLASS2_ID_2_MOD_EXP]} ]}]}).

-define(NC_DEL, [{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
		 {'nc:operation',"delete"}]).

-define(TEST_CLASS1_2_DEL, {?'ManagedElement',
			    ?COM_TOP,
			    [?ELEMENT_ID_1,
			     {?TEST_ROOT,
			      ?TEST_MOM,
			      [?TEST_ROOT_ID_1,
			       {?TEST_CLASS1,
				?NC_DEL,
				[?TEST_CLASS1_ID_2]}]}]}).

-define(TEST_CLASS2_2_DEL, {?'ManagedElement',
			    ?COM_TOP,
			    [?ELEMENT_ID_1,
			     {?TEST_ROOT,
			      ?TEST_MOM,
			      [?TEST_ROOT_ID_1,
			       {?TEST_CLASS2,
				?NC_DEL,
				[{'testClass2Id',[],["2"]}]}]}]}).

-define(TEST_CLASS1_2_MOD, {?'ManagedElement',
			    ?COM_TOP,
			    [?ELEMENT_ID_1,
			     {?TEST_ROOT,
			      ?TEST_MOM,
			      [?TEST_ROOT_ID_1,
			       {?TEST_CLASS1,
				[],
				[?TEST_CLASS1_ID_2, {int32,[],["65"]}]}]}]}).

-define(TEST_CLASS2_2_MOD, {?'ManagedElement',
			    ?COM_TOP,
			    [?ELEMENT_ID_1,
			     {?TEST_ROOT,
			      ?TEST_MOM,
			      [?TEST_ROOT_ID_1,
			       {?TEST_CLASS2,
				[],
				[?TEST_CLASS2_ID_2_MOD]}]}]}).

-define(ALL_TEST_CLASS, {?'ManagedElement',
			?COM_TOP,
			[?ELEMENT_ID_1,
			 {?TEST_ROOT,
			  ?TEST_MOM,
			  [?TEST_ROOT_ID_1]}]}).

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_netconf, cth_conn_log, rct_safe_imm_oi_rpc
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {rct_netconf,nc1},
		 %% {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
		 %% 			       ["sa_ais_err_bad_operation"]}}]}},
		 {rct_logging, {oi_testapp,
				[{erlang,{["ERROR REPORT",
					   "CRASH REPORT"],
					  ["sa_ais_err_bad_operation"]}}]}},
		 {rct_core,[]},
		 {cth_conn_log,[]},
		 {rct_safe_imm_oi_rpc,[{finalize_on_fail,true}]}
		]}].

%% @hidden
init_per_suite(Config) ->
    %% fake VNFM for vrcs
    case rct_rpc:call(rpc, sysEnv, rcs_mode_2, [], 10000, noprint) of
        vrcs ->
            rct_rpc:call(rpc, os, putenv, ["FAKE_VNFM", ""], 10000, noprint);
        _ ->
            rct_rpc:call(rpc, os, unsetenv, ["FAKE_VNFM"], 10000, noprint)
    end,
    Config.

%% @hidden
end_per_suite(_Config) ->
    %% fake VNFM for vrcs
    case rct_rpc:call(rpc, sysEnv, rcs_mode_2, [], 10000, noprint) of
        vrcs ->
            rct_rpc:call(rpc, os, unsetenv, ["FAKE_VNFM"], 10000, noprint);
        _ ->
            ok
    end,
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
    	    ct:pal("Testcase failed due to: ~p.  \nClean up added rbs units",
		   [Reason]),

	    OI_1 = rct_rpc:call(rpc,
				safs_imm_db,
				ci_get,
				['TESTMOMTestClass1'],
				10000,
				noprint),
	    ct:pal("OI_1: ~p.", [OI_1]),
	    OI_2 = rct_rpc:call(rpc,
				safs_imm_db,
				ci_get,
				['TESTMOMTestClass2'],
				10000,
				noprint),
	    ct:pal("OI_2: ~p.", [OI_2]),

	    ct_netconfc:open(nc1, []),
	    ct_netconfc:edit_config(nc1, running, ?TEST_CLASS1_2_DEL),
	    ct_netconfc:close_session(nc1),
	    ct_netconfc:open(nc1, []),
	    ct_netconfc:edit_config(nc1, running, ?TEST_CLASS2_2_DEL),
	    ct_netconfc:close_session(nc1)
    end,
    ok.
%% @hidden
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
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     test_class1_create_mod_delete,
     test_class1_create_err,
     test_class_1_2_create_mod_delete_2oi,
     test_class_1_2_create_err_2oi%% ,
     %% test_class_2_1_create_err_2oi
    ].


%%--------------------------------------------------------------------
%% @doc
%% Test create, modify and delete of an object of TestClass1 with ok result.
%% Verify that the correct OI callbacks are executed.
%% <br/><br/>
%% @spec test_class1_create_mod_delete(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_class1_create_mod_delete(_Config) ->
    CbFun = get_cb_fun(),
    Handle = create_oi_class_impl(CbFun, ?IMPL_NAME_1, ?IMM_TEST_CLASS1),

    try
	{ok, _} = ct_netconfc:open(nc1, []),
	ok = ct_netconfc:edit_config(nc1,running, ?TEST_CLASS1_2),
	{ok, [?TEST_CLASS1_2_EXP]} = ct_netconfc:get(nc1, ?TEST_CLASS1_2),
	ct_netconfc:close_session(nc1),

	ok = expect_callback(?SAFE_CCB_CB_CREATE),
	%% ok = expect_callback(?SAFE_CCB_CB_MOD),
	ok = expect_callback(?SAFE_CCB_CB_COMP),
	ok = expect_callback(?SAFE_CCB_CB_APP),

	timer:sleep(100),
	{ok, _} = ct_netconfc:open(nc1, []),
	ok = ct_netconfc:edit_config(nc1,running, ?TEST_CLASS1_2_MOD),
%        {ok, [?TEST_CLASS1_2_EXP]} = ct_netconfc:get(nc1, ?TEST_CLASS1_2),
	{ok, [{'ManagedElement',
	       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	       [{managedElementId,[],["1"]},
		{'TestRoot',
		 [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
		 [{testRootId,[],["1"]},
		  {'TestClass1',[],
		   [{int32,[],["65"]},{testClass1Id,[],["2"]}]}]}]}]} = ct_netconfc:get(nc1, ?TEST_CLASS1_2),
	ct_netconfc:close_session(nc1),

	ok = expect_callback(?SAFE_CCB_CB_MOD),
	ok = expect_callback(?SAFE_CCB_CB_COMP),
	ok = expect_callback(?SAFE_CCB_CB_APP),

	timer:sleep(100),
	{ok, _} = ct_netconfc:open(nc1, []),
	ok = ct_netconfc:edit_config(nc1, running, ?TEST_CLASS1_2_DEL),
	{error, _} = ct_netconfc:get(nc1, ?TEST_CLASS1_2),
	ct_netconfc:close_session(nc1),

	ok = expect_callback(?SAFE_CCB_CB_DEL),
	ok = expect_callback(?SAFE_CCB_CB_COMP),
	ok = expect_callback(?SAFE_CCB_CB_APP),

	timer:sleep(100),
	{ok, _} = ct_netconfc:open(nc1, []),
	{error, _} = ct_netconfc:get(nc1, ?TEST_CLASS1_2),
	ct_netconfc:close_session(nc1),

	delete_class_impl_and_oi([{?IMM_TEST_CLASS1, ?IMPL_NAME_1, Handle}])

    catch
	_:Reason ->
	    ct:pal("TC failed, Reason: ~p", [Reason]),
	    cleanup_class_impl_and_oi([{?IMM_TEST_CLASS1, ?IMPL_NAME_1, Handle}]),
	    ct:fail(Reason)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Test create of an object of TestClass1 where the OI callback returns error.
%% Verify that the corresponding OI callbacks are executed.
%% - Only one CCB_CB_CREATE shall be recieved, due to no unnecessary informations is sent.
%% <br/><br/>
%% @spec test_class1_create_err(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_class1_create_err(_Config) ->
    CbFunErr = get_cb_fun(?SAFE_AIS_ERR_BAD_OPERATION),
    Handle = create_oi_class_impl(CbFunErr, ?IMPL_NAME_1, ?IMM_TEST_CLASS1),

    try
	{ok, _} = ct_netconfc:open(nc1, []),
	ok = ct_netconfc:edit_config(nc1,running, ?TEST_CLASS1_2),
	{ok, [?TEST_CLASS1_2_EXP]} = ct_netconfc:get(nc1, ?TEST_CLASS1_2),
	ct_netconfc:close_session(nc1),
	ok = expect_callback(?SAFE_CCB_CB_CREATE),
	ok = expect_callback(?SAFE_CCB_CB_ABORT),
	timer:sleep(100),
	{ok, _} = ct_netconfc:open(nc1, []),
	{error, _} = ct_netconfc:get(nc1, ?TEST_CLASS1_2),
	ct_netconfc:close_session(nc1),

	delete_class_impl_and_oi([{?IMM_TEST_CLASS1, ?IMPL_NAME_1, Handle}])

    catch
	_:Reason ->
	    ct:pal("TC failed, Reason: ~p", [Reason]),
	    cleanup_class_impl_and_oi([{?IMM_TEST_CLASS1, ?IMPL_NAME_1, Handle}]),
	    ct:fail(Reason)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Test create, modify and delete of object of TestClass1 and TestClass2 with ok result.
%% Verify that the correct OI callbacks are executed.
%% <br/><br/>
%% @spec test_class_1_2_create_mod_delete_2oi(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_class_1_2_create_mod_delete_2oi(_Config) ->
    CbFunOk1 = get_cb_fun(),
    Handle1 = create_oi_class_impl(CbFunOk1, ?IMPL_NAME_1, ?IMM_TEST_CLASS1),

    %% CbFunOk2 = get_cb_fun(),
    %% Handle2 = create_oi_class_impl(CbFunOk2, ?IMPL_NAME_2, ?IMM_TEST_CLASS2),

    try
	{ok, _} = ct_netconfc:open(nc1, []),
	ok = ct_netconfc:edit_config(nc1,running, ?TEST_CLASS1_2),
	ok = ct_netconfc:edit_config(nc1,running, ?TEST_CLASS2_2),
	{ok, [?TEST_CLASS1_2_EXP]} = ct_netconfc:get(nc1, ?TEST_CLASS1_2),
	{ok, [?TEST_CLASS2_2_EXP]} = ct_netconfc:get(nc1, ?TEST_CLASS2_2),
	ct_netconfc:close_session(nc1),

	ok = expect_callback(?SAFE_CCB_CB_CREATE),
	%% ok = expect_callback(?SAFE_CCB_CB_MOD),
	%% ok = expect_callback(?SAFE_CCB_CB_CREATE),
	%% ok = expect_callback(?SAFE_CCB_CB_MOD),
	ok = expect_callback(?SAFE_CCB_CB_COMP),
	%% ok = expect_callback(?SAFE_CCB_CB_COMP),
	ok = expect_callback(?SAFE_CCB_CB_APP),
	%% ok = expect_callback(?SAFE_CCB_CB_APP),

	timer:sleep(100),
	{ok, _} = ct_netconfc:open(nc1, []),
	{ok, [?TEST_CLASS1_2_EXP]} = ct_netconfc:get(nc1, ?TEST_CLASS1_2),
	{ok, [?TEST_CLASS2_2_EXP]} = ct_netconfc:get(nc1, ?TEST_CLASS2_2),
	ct_netconfc:close_session(nc1),

	timer:sleep(100),
	{ok, _} = ct_netconfc:open(nc1, []),
	ok = ct_netconfc:edit_config(nc1,running, ?TEST_CLASS1_2_MOD),
	ok = ct_netconfc:edit_config(nc1,running, ?TEST_CLASS2_2_MOD),
	ct_netconfc:close_session(nc1),

	{ok, _} = ct_netconfc:open(nc1, []),
%	{ok, [?TEST_CLASS1_2_MOD_EXP]} = ct_netconfc:get(nc1, ?TEST_CLASS1_2),
	{ok, [{'ManagedElement',
	       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	       [{managedElementId,[],["1"]},
		{'TestRoot',
		 [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
		 [{testRootId,[],["1"]},
		  {'TestClass1',[],
		   [{int32,[],["65"]},{testClass1Id,[],["2"]}]}]}]}]} = ct_netconfc:get(nc1, ?TEST_CLASS1_2),
%	{ok, [?TEST_CLASS2_2_MOD_EXP]} = ct_netconfc:get(nc1, ?TEST_CLASS2_2_GET),
	{ok, [{'ManagedElement',
	       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	       [{managedElementId,[],["1"]},
		{'TestRoot',
		 [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
		 [{testRootId,[],["1"]},
		  {'TestClass2',[],
		   [{struct2,
		     [{struct,"Struct2"}],
		     [{struct2mem1,[],["init"]},{struct2key,[],["3"]}]},
		    {testClass2Id,[],["2"]},
		    {'TestClass3',[],
		     [{testClass3Id,[],["2"]},
		      {managedElement,[],["helvete"]}]}]}]}]}]} = ct_netconfc:get(nc1, ?TEST_CLASS2_2_GET),
	ct_netconfc:close_session(nc1),

	ok = expect_callback(?SAFE_CCB_CB_MOD),
	%% ok = expect_callback(?SAFE_CCB_CB_MOD),
	ok = expect_callback(?SAFE_CCB_CB_COMP),
	%% ok = expect_callback(?SAFE_CCB_CB_COMP),
	ok = expect_callback(?SAFE_CCB_CB_APP),
	%% ok = expect_callback(?SAFE_CCB_CB_APP),

	timer:sleep(100),
	{ok, _} = ct_netconfc:open(nc1, []),
	ok = ct_netconfc:edit_config(nc1, running, ?TEST_CLASS1_2_DEL),
      	ok = ct_netconfc:edit_config(nc1, running, ?TEST_CLASS2_2_DEL),
	ct_netconfc:close_session(nc1),

	ok = expect_callback(?SAFE_CCB_CB_DEL),
      	%% ok = expect_callback(?SAFE_CCB_CB_DEL),
	ok = expect_callback(?SAFE_CCB_CB_COMP),
	%% ok = expect_callback(?SAFE_CCB_CB_COMP),
	ok = expect_callback(?SAFE_CCB_CB_APP),
	%% ok = expect_callback(?SAFE_CCB_CB_APP),

	timer:sleep(100),
	{ok, _} = ct_netconfc:open(nc1, []),
	{error, _} = ct_netconfc:get(nc1, ?TEST_CLASS1_2),
	{error, _} = ct_netconfc:get(nc1, ?TEST_CLASS2_2),
	ct_netconfc:close_session(nc1),

	%% delete_class_impl_and_oi([{?IMM_TEST_CLASS1, ?IMPL_NAME_1, Handle1},
	%% 			  {?IMM_TEST_CLASS2, ?IMPL_NAME_2, Handle2}])
	delete_class_impl_and_oi([{?IMM_TEST_CLASS1, ?IMPL_NAME_1, Handle1}])

    catch
	_:Reason ->
	    ct:pal("TC fail, Reason: ~p", [Reason]),
	    %% cleanup_class_impl_and_oi([{?IMM_TEST_CLASS1, ?IMPL_NAME_1, Handle1},
	    %% 			       {?IMM_TEST_CLASS2, ?IMPL_NAME_2, Handle2}]),
	    cleanup_class_impl_and_oi([{?IMM_TEST_CLASS1, ?IMPL_NAME_1, Handle1}]),
	    ct:fail(Reason)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Create two class implementer, one for TestClass1 and one for TestClass2.
%% Test create of object of TestClass1 and TestClass2,
%% where the OI callback returns error from TestClass1 and ok from TestClass2.
%% Verify that the corresponding OI callbacks are executed.
%% - Only one CCB_CB_CREATE shall be recieved, due to no unnecessary informations is sent.
%% <br/><br/>
%% @spec test_class_1_2_create_err_2oi(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_class_1_2_create_err_2oi(_Config) ->
    CbFunErr = get_cb_fun(?SAFE_AIS_ERR_BAD_OPERATION),
    Handle1 = create_oi_class_impl(CbFunErr, ?IMPL_NAME_1, ?IMM_TEST_CLASS1),

    CbFunOk = get_cb_fun(),
    Handle2 = create_oi_class_impl(CbFunOk, ?IMPL_NAME_2, ?IMM_TEST_CLASS2),

    try
	{ok, _} = ct_netconfc:open(nc1, []),
	ok = ct_netconfc:edit_config(nc1,running, ?TEST_CLASS1_2),
	ok = ct_netconfc:edit_config(nc1,running, ?TEST_CLASS2_2),
	ct_netconfc:close_session(nc1),
	ok = expect_callback(?SAFE_CCB_CB_CREATE),
	ok = expect_callback(?SAFE_CCB_CB_ABORT),
	%% {error, "Failed to receive CB Args"} = expect_callback(?SAFE_CCB_CB_CREATE),
	%% {error, "Failed to receive CB Args"} = expect_callback(?SAFE_CCB_CB_ABORT),
	%% {error, "Failed to receive CB Args"} = expect_callback(?SAFE_CCB_CB_ABORT),

	timer:sleep(100),
	{ok, _} = ct_netconfc:open(nc1, []),
	{error, _} = ct_netconfc:get(nc1, ?TEST_CLASS1_2),
	{error, _} = ct_netconfc:get(nc1, ?TEST_CLASS2_2),
	ct_netconfc:close_session(nc1),

	delete_class_impl_and_oi([{?IMM_TEST_CLASS1, ?IMPL_NAME_1, Handle1},
				  {?IMM_TEST_CLASS2, ?IMPL_NAME_2, Handle2}])

    catch
	_:Reason ->
	    ct:pal("TC fail, Reason: ~p", [Reason]),
	    cleanup_class_impl_and_oi([{?IMM_TEST_CLASS1, ?IMPL_NAME_1, Handle1},
				       {?IMM_TEST_CLASS2, ?IMPL_NAME_2, Handle2}]),

	    ct:fail(Reason)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Create two class implementer, one for TestClass1 and one for TestClass2.
%% Test create of object of TestClass2 and TestClass1,
%% where the OI callback returns error from TestClass1 and ok from TestClass2.
%% Verify that the corresponding OI callbacks are executed.
%% - Only one CCB_CB_ABORT shall be recieved, due to no unnecessary informations is sent.
%% <br/><br/>
%% @spec test_class_2_1_create_err_2oi(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_class_2_1_create_err_2oi(_Config) ->
    ErrorString = "Test case wants to announce that the operation failed",
    CbFunErr = get_cb_fun(?SAFE_AIS_ERR_BAD_OPERATION, ErrorString),
    Handle1 = create_oi_class_impl(CbFunErr, ?IMPL_NAME_1, ?IMM_TEST_CLASS1),

    CbFunOk = get_cb_fun(),
    Handle2 = create_oi_class_impl(CbFunOk, ?IMPL_NAME_2, ?IMM_TEST_CLASS2),

    try
	{ok, _} = ct_netconfc:open(nc1, []),
	ok = ct_netconfc:edit_config(nc1,running, ?TEST_CLASS2_2),
	ok = ct_netconfc:edit_config(nc1,running, ?TEST_CLASS1_2),
	{error, ErrorInfo} = ct_netconfc:close_session(nc1),
	ok = expect_callback(?SAFE_CCB_CB_CREATE),
	ok = expect_callback(?SAFE_CCB_CB_CREATE),
	ok = expect_callback(?SAFE_CCB_CB_ABORT),
	ok = expect_callback(?SAFE_CCB_CB_ABORT),
	ok = verify_error_message(ErrorInfo, ErrorString),

	timer:sleep(100),
	{ok, _} = ct_netconfc:open(nc1, []),
	{error, _} = ct_netconfc:get(nc1, ?TEST_CLASS1_2),
	{error, _} = ct_netconfc:get(nc1, ?TEST_CLASS2_2),
	ct_netconfc:close_session(nc1),

	delete_class_impl_and_oi([{?IMM_TEST_CLASS1, ?IMPL_NAME_1, Handle1},
				  {?IMM_TEST_CLASS2, ?IMPL_NAME_2, Handle2}])

    catch
	_:Reason ->
	    ct:pal("TC fail, Reason: ~p", [Reason]),
	    cleanup_class_impl_and_oi([{?IMM_TEST_CLASS1, ?IMPL_NAME_1, Handle1},
				       {?IMM_TEST_CLASS2, ?IMPL_NAME_2, Handle2}]),

	    ct:fail(Reason)
    end.


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
get_cb_fun() ->
    cb_fun(ok, undefined).


get_cb_fun(SafeErr) ->
    cb_fun({error, SafeErr}, undefined).


get_cb_fun(SafeErr, ErrorString) ->
    cb_fun({error, SafeErr}, ErrorString).


cb_fun(Res, ErrorString) ->
    Self = self(),
    fun(Arg) ->
	    case ErrorString of
		undefined ->
		    ct:pal("Executing ~p", [element(1, Arg)]);
		_ ->
		    ct:pal("Executing ~p~nand ccb_set_err_string",
			   [element(1, Arg)]),
		    Handle = element(2, Arg),
		    CcbId = element(3, Arg),
		    rct_safe_imm_oi_rpc:ccb_set_error_string(Handle,
							     CcbId,
							     ErrorString)
	    end,
	    Self ! {oi_ccb_callback, Arg},
	    ct:log("Return result: ~p.", [Res]),
	    Res
    end.

expect_callback(ExpectedCb) ->
    case receive_callback_arg() of
	{ok, Arg} ->
	    expect_callback(ExpectedCb, element(1, Arg));
	Error ->
	    Error
    end.

expect_callback(ExpectedCb,  ExpectedCb) ->
    ok;

expect_callback(ExpectedCb, OtherCb) ->
    Reason = lists:flatten(io_lib:format("Expected Callback ~p, Got ~p",
					 [ExpectedCb, OtherCb])),
    ct:fail(Reason).


receive_callback_arg() ->
    receive
	{oi_ccb_callback, Arg} ->
	    ct:pal("Received ~p Args:~n~p", [element(1,Arg),Arg]),
	    {ok, Arg}
    after 5000 ->
	    ct:pal("Failed to receive CB Args"),
	    {error, "Failed to receive CB Args"}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_oi_class_impl(CbFun, IMPL_NAME, IMM_TEST_CLASS) ->
    ct:pal("Initialize OI Handle", []),
    {ok, Handle, Vsn} = rct_safe_imm_oi_rpc:initialize_2(CbFun),
    ct:pal("Initialize ok: ~p ~p",[Handle, Vsn]),
    ct:pal("Set Implementer name: ~p", [IMPL_NAME]),
    ok = rct_safe_imm_oi_rpc:implementer_set(Handle, IMPL_NAME),
    ct:pal("Set Class Implementer for: ~p", [IMM_TEST_CLASS]),
    ok = rct_safe_imm_oi_rpc:class_implementer_set(Handle, IMM_TEST_CLASS),
    Handle.

delete_class_impl_and_oi(DeleteData) ->
    lists:foreach(fun({IMM_TEST_CLASS, IMPL_NAME, Handle}) ->
			  ct:pal("Release Class Implementer for: ~p", [IMM_TEST_CLASS]),
			  ok = rct_safe_imm_oi_rpc:class_implementer_release(Handle, IMM_TEST_CLASS),
			  ct:pal("Clear Implementer ~p", [IMPL_NAME]),
			  ok = rct_safe_imm_oi_rpc:implementer_clear(Handle),
			  ct:pal("Finalize OI Handle ~p", [Handle]),
			  ok = rct_safe_imm_oi_rpc:finalize(Handle)
		  end, DeleteData).

cleanup_class_impl_and_oi(CleanUpData) ->
    lists:foreach(fun({IMM_TEST_CLASS, IMPL_NAME, Handle}) ->
			  ct:pal("Release Class Implementer for: ~p", [IMM_TEST_CLASS]),
			  rct_safe_imm_oi_rpc:class_implementer_release(Handle, IMM_TEST_CLASS),
			  ct:pal("Clear Implementer ~p", [IMPL_NAME]),
			  rct_safe_imm_oi_rpc:implementer_clear(Handle),
			  ct:pal("Finalize OI Handle ~p", [Handle]),
			  rct_safe_imm_oi_rpc:finalize(Handle)
		  end, CleanUpData).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
verify_error_message(ErrorInfo, ExpectedString) ->
    verify_error_message(ErrorInfo, ExpectedString, ErrorInfo).

verify_error_message([{'error-message', _, [ReturnString]} | Tail],
		     ExpectedString,
		     ErrorInfo) ->
    case string:str(ReturnString, ExpectedString) of
	0 ->
	    %% Sub-string (ExpString) not found in ReturnString
	    verify_error_message(Tail, ExpectedString, ErrorInfo);
	_ ->
	    ct:pal("Verified that set ERR string was returned:~n~p",
		   [ExpectedString]),
	    ok
    end;
verify_error_message([_ | Tail], ExpectedString, ErrorInfo) ->
    verify_error_message(Tail, ExpectedString, ErrorInfo);
verify_error_message([], _, ErrorInfo) ->
    ct:fail({error_string_not_returned, ErrorInfo}).

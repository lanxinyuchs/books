%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	safs_imm_oi_SUITE.erl %
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R4A/4

%%% @doc 
%%% == Basic test of safs imm_oi ==
%%%  
%%% @end
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
%%% R2A/1      2013-08-12 etxivri     Ported some TCs from safs_imm_oi_SUITE in git env.
%%% R2A/3      2013-09-10 etxivri     Updates due to renamed saf-rpc hooks.
%%% R2A/3      2014-03-26 etxivri     Updaet to get erlang log from testnode.
%%% R4A/1      2015-07-02 etxivri     Add sleep to make it more robust.
%%% R4A/2      2015-07-13 etxjovp     Add group definitions used by CS CI
%%% R4A/3      2015-09-14 etxivri     Removed sleep.
%%% R4A/4      2015-10-11 etxivri     Removed export_all due to it reluts in compile warnings in OTP20
%%% ----------------------------------------------------------
%%% 
-module(safs_imm_oi_SUITE).

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([all/0, 
	 suite/0,
	 init_per_suite/1, 
	 end_per_suite/1, 
         init_per_testcase/2, 
	 end_per_testcase/2,
	 init_per_group/2,
	 end_per_group/2,
	 tc_safe_lc/1,
	 tc_safe_lc_multi/1,
	 tc_safe_implfcn/1, 
	 tc_safe_implfcn_multi/1, 
	 tc_safe_implfcn_err/1,
	 tc_safe_implfcn_multi_err/1,
	 tc_safe_implfcn_rt_err/1,
         groups/0
	]).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include_lib("safe/include/safe.hrl").
-include_lib("safe/include/safe_imm.hrl").

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------
-define(IMPL_NAME_1, "Nisse").
-define(IMPL_NAME_2, "Arne").
-define(CONF_CLASS_1, "TESTMOMTestClass1").
-define(CONF_CLASS_2, "TESTMOMTestClass2").
-define(CONF_OBJ_1, "testClass1Id=1,TESTMOMtestRootId=1").
-define(CONF_OBJ_2, "testClass2Id=1,TESTMOMtestRootId=1").

-define(CLASS_PARENT, "TESTMOMtestRootId=1").
-define(RT_CLASS, "TESTMOMTestClass5").
-define(RT_OBJ, "testClass5Id=rt_obj,TESTMOMtestRootId=1").
-define(RT_OBJ_ATTR, #safe_imm_attr_values_2{attr_name = "testClass5Id",
					     attr_value_type = 9, %STRING
					     attr_values = ["rt_obj"]}).
-define(NON_EXISTING_OBJ, "NonExistingObj=myNonExistingObj").

%%======================================================================
%% Initialization functions.
%%======================================================================
%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{minutes,10}},
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {rct_netconf,nc1},
		 %% {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
		 {rct_logging, 
		  {oi_testapp, [{erlang,{["ERROR REPORT",
					  "CRASH REPORT"],[]}}]}},
		 {rct_core,[]},
		 {cth_conn_log,[]},
		 {rct_safe_imm_oi_rpc,[{safe_debug_level, 0}, {finalize_on_fail,true}]}]}
    ].

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
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
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

%%====================================================================
%% SUITE specification
%%====================================================================
%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [tc_safe_lc,
     tc_safe_lc_multi,
     tc_safe_implfcn, 
     tc_safe_implfcn_multi, 
     tc_safe_implfcn_err,
     tc_safe_implfcn_multi_err,
     tc_safe_implfcn_rt_err].

%%====================================================================
%% Test Cases
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% lifecycle. <br/>
%% @spec tc_safe_lc(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
tc_safe_lc(_Config) ->
    {ok, Handle, Vsn} = rct_safe_imm_oi_rpc:initialize_2(undefined),
    ct:pal("safe Handle = ~p, Vsn = ~p", [Handle, Vsn]),
    try
	ok = rct_safe_imm_oi_rpc:finalize(Handle)
    catch
	_:Reason ->
	    rct_safe_imm_oi_rpc:finalize(Handle),
	    
	    ct:fail(Reason)
    end.
%%--------------------------------------------------------------------
%% @doc
%% lifecycle multi. <br/>
%% @spec tc_safe_lc_multi(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
tc_safe_lc_multi(_Config) ->
    {ok, Handle1, Vsn1} = rct_safe_imm_oi_rpc:initialize_2(undefined),
    ct:pal("First safe Handle = ~p, Vsn = ~p", [Handle1, Vsn1]),
    {ok, Handle2, Vsn2} = rct_safe_imm_oi_rpc:initialize_2(undefined),
    ct:pal("Second safe Handle = ~p, Vsn = ~p", [Handle2, Vsn2]),
    try
	ok = rct_safe_imm_oi_rpc:finalize(Handle1),
	ok = rct_safe_imm_oi_rpc:finalize(Handle2)
    catch
	_:Reason ->
	    rct_safe_imm_oi_rpc:finalize(Handle1),
	    rct_safe_imm_oi_rpc:finalize(Handle2),

	    ct:fail(Reason)
    end.
%%--------------------------------------------------------------------
%% @doc
%% implementer functions. <br/>
%% @spec tc_safe_implfcn(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
tc_safe_implfcn(_Config) ->
    {ok, Handle, Vsn} = rct_safe_imm_oi_rpc:initialize_2(undefined),
    ct:pal("safe Handle = ~p, Vsn = ~p", [Handle, Vsn]),
    try
	ok = rct_safe_imm_oi_rpc:implementer_set(Handle, ?IMPL_NAME_1),
	ok = rct_safe_imm_oi_rpc:class_implementer_set(Handle, ?CONF_CLASS_1),
	ok = rct_safe_imm_oi_rpc:object_implementer_set(Handle, 
						    ?CONF_OBJ_2,
						    ?SAFE_IMM_ONE),
	
	ok = rct_safe_imm_oi_rpc:object_implementer_release(Handle, 
							?CONF_OBJ_2,
							?SAFE_IMM_ONE),
	ok = rct_safe_imm_oi_rpc:class_implementer_release(Handle, 
						       ?CONF_CLASS_1),
	
	ok = rct_safe_imm_oi_rpc:implementer_clear(Handle),
	ok = rct_safe_imm_oi_rpc:finalize(Handle)
    catch
	_:Reason ->
	    rct_safe_imm_oi_rpc:object_implementer_release(Handle, 
						       ?CONF_OBJ_2,
						       ?SAFE_IMM_ONE),
	    rct_safe_imm_oi_rpc:class_implementer_release(Handle, 
						      ?CONF_CLASS_1),
	    rct_safe_imm_oi_rpc:implementer_clear(Handle),
	    rct_safe_imm_oi_rpc:finalize(Handle),

	    ct:fail(Reason)
    end.

%%--------------------------------------------------------------------
%% @doc
%% implementer functions multi.<br/>
%% @spec tc_safe_implfcn_multi(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
tc_safe_implfcn_multi(_Config) ->
    {ok, Handle, Vsn} = rct_safe_imm_oi_rpc:initialize_2(undefined),
    ct:pal("safe Handle = ~p, Vsn = ~p", [Handle, Vsn]),
    {ok, Handle2, Vsn2} = rct_safe_imm_oi_rpc:initialize_2(undefined),
    ct:pal("Second safe Handle = ~p, Vsn = ~p", [Handle2, Vsn2]),
    try
	ok = rct_safe_imm_oi_rpc:implementer_set(Handle, ?IMPL_NAME_1),
	ok = rct_safe_imm_oi_rpc:implementer_set(Handle2, ?IMPL_NAME_2),
	ok = rct_safe_imm_oi_rpc:class_implementer_set(Handle, ?CONF_CLASS_1),
	ok = rct_safe_imm_oi_rpc:class_implementer_set(Handle2, ?CONF_CLASS_2),
	ok = rct_safe_imm_oi_rpc:class_implementer_release(Handle, ?CONF_CLASS_1),
	ok = rct_safe_imm_oi_rpc:class_implementer_release(Handle2, ?CONF_CLASS_2),
	ok = rct_safe_imm_oi_rpc:object_implementer_set(Handle, 
						    ?CONF_OBJ_1,
						    ?SAFE_IMM_ONE),
	ok = rct_safe_imm_oi_rpc:object_implementer_set(Handle2, 
						    ?CONF_OBJ_2,
						    ?SAFE_IMM_ONE),
	ok = rct_safe_imm_oi_rpc:object_implementer_release(Handle, 
							?CONF_OBJ_1,
							?SAFE_IMM_ONE),
	ok = rct_safe_imm_oi_rpc:object_implementer_release(Handle2, 
							?CONF_OBJ_2,
							?SAFE_IMM_ONE),
	ok = rct_safe_imm_oi_rpc:implementer_clear(Handle),
	ok = rct_safe_imm_oi_rpc:implementer_clear(Handle2),
	ok = rct_safe_imm_oi_rpc:finalize(Handle),
	ok = rct_safe_imm_oi_rpc:finalize(Handle2)
    catch
	_:Reason ->
	    rct_safe_imm_oi_rpc:object_implementer_release(Handle, 
						       ?CONF_OBJ_1,
						       ?SAFE_IMM_ONE),
	    rct_safe_imm_oi_rpc:object_implementer_release(Handle2, 
						       ?CONF_OBJ_2,
						       ?SAFE_IMM_ONE),
	    rct_safe_imm_oi_rpc:class_implementer_release(Handle, ?CONF_CLASS_1),
	    rct_safe_imm_oi_rpc:class_implementer_release(Handle2, ?CONF_CLASS_2),
	    rct_safe_imm_oi_rpc:implementer_clear(Handle),
	    rct_safe_imm_oi_rpc:implementer_clear(Handle2),
	    rct_safe_imm_oi_rpc:finalize(Handle),
	    rct_safe_imm_oi_rpc:finalize(Handle2),

	    ct:fail(Reason)
    end.

%%--------------------------------------------------------------------
%% @doc
%% implementer functions error.<br/>
%% @spec tc_safe_implfcn_err(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
tc_safe_implfcn_err(_Config) ->
    %% Initialize with no callbacks
    {ok, Handle1, Vsn1} = rct_safe_imm_oi_rpc:initialize_2(undefined),
    ct:pal("Initialize: safe Handle = ~p, Vsn = ~p", [Handle1, Vsn1]),
    try
	%% Try to set class implementer before implementer name is set.
	{error, ?SAFE_AIS_ERR_BAD_HANDLE} = 
	    rct_safe_imm_oi_rpc:class_implementer_set(Handle1, ?CONF_CLASS_1),
	
	%% Try to release class implementer before implementer name is set.
	{error, ?SAFE_AIS_ERR_BAD_HANDLE} = 
	    rct_safe_imm_oi_rpc:class_implementer_release(Handle1, ?CONF_CLASS_1),
	
	%% Try to set object implementer before implementer name is set.
	{error, ?SAFE_AIS_ERR_BAD_HANDLE} = 
	    rct_safe_imm_oi_rpc:object_implementer_set(Handle1, 
						   ?CONF_OBJ_2,
						   ?SAFE_IMM_ONE),
	%% Try to release object implementer before implementer name is set.
	{error, ?SAFE_AIS_ERR_BAD_HANDLE} = 
	    rct_safe_imm_oi_rpc:object_implementer_release(Handle1, 
						       ?CONF_OBJ_2,
						       ?SAFE_IMM_ONE),
	%% Set Implementer Name
	ok = rct_safe_imm_oi_rpc:implementer_set(Handle1, ?IMPL_NAME_1),
	
	%% Try to set another Implementer Name for the same Handle 
	{error, ?SAFE_AIS_ERR_EXIST} = 
	    rct_safe_imm_oi_rpc:implementer_set(Handle1, ?IMPL_NAME_2),
	
	%% Try to set class implementer for non existing class
	{error, ?SAFE_AIS_ERR_NOT_EXIST} = 
	    rct_safe_imm_oi_rpc:class_implementer_set(Handle1, "NonExistingClass"),
	
	%% Set class implementer
	ok = rct_safe_imm_oi_rpc:class_implementer_set(Handle1, ?CONF_CLASS_1),
	
	%% Set class implementer again with the same Handle and the same class
	ok = rct_safe_imm_oi_rpc:class_implementer_set(Handle1, ?CONF_CLASS_1),
	
	%% Try to set object implementer for non existing object
	{error, ?SAFE_AIS_ERR_NOT_EXIST} = 
	    rct_safe_imm_oi_rpc:object_implementer_set(Handle1, 
						   ?NON_EXISTING_OBJ,
						   ?SAFE_IMM_ONE),
	{error, ?SAFE_AIS_ERR_NOT_EXIST} = 
	    rct_safe_imm_oi_rpc:object_implementer_set(Handle1, 
						   ?NON_EXISTING_OBJ,
						   ?SAFE_IMM_SUBLEVEL),
	{error, ?SAFE_AIS_ERR_NOT_EXIST} = 
	    rct_safe_imm_oi_rpc:object_implementer_set(Handle1, 
						   ?NON_EXISTING_OBJ,
						   ?SAFE_IMM_SUBTREE),
	
	%% Set object implementer with various scope
	ok = rct_safe_imm_oi_rpc:object_implementer_set(Handle1, 
						    ?CONF_OBJ_2,
						    ?SAFE_IMM_ONE),
	ok = rct_safe_imm_oi_rpc:object_implementer_set(Handle1, 
						    ?CONF_OBJ_2,
						    ?SAFE_IMM_SUBLEVEL),
	ok = rct_safe_imm_oi_rpc:object_implementer_set(Handle1, 
						    ?CONF_OBJ_2,
						    ?SAFE_IMM_SUBTREE),
	
	%% Set the same object implementer again in different order
	ok = rct_safe_imm_oi_rpc:object_implementer_set(Handle1, 
						    ?CONF_OBJ_2,
						    ?SAFE_IMM_SUBTREE),
	ok = rct_safe_imm_oi_rpc:object_implementer_set(Handle1, 
						    ?CONF_OBJ_2,
						    ?SAFE_IMM_SUBLEVEL),
	ok = rct_safe_imm_oi_rpc:object_implementer_set(Handle1, 
						    ?CONF_OBJ_2,
						    ?SAFE_IMM_ONE),
	
	%% Release object implementer with scope subtree
	ok = rct_safe_imm_oi_rpc:object_implementer_release(Handle1, 
							?CONF_OBJ_2,
							?SAFE_IMM_SUBTREE),

	%% Try to release same object implementer with scope one
	{error, ?SAFE_AIS_ERR_NOT_EXIST} = 
	    rct_safe_imm_oi_rpc:object_implementer_release(Handle1, 
						       ?CONF_OBJ_2,
						       ?SAFE_IMM_ONE),
	
	%% Clean up after test
	ok = rct_safe_imm_oi_rpc:class_implementer_release(Handle1, ?CONF_CLASS_1),
	ok = rct_safe_imm_oi_rpc:implementer_clear(Handle1),
	ok = rct_safe_imm_oi_rpc:finalize(Handle1)
    catch
	_:Reason ->
	    rct_safe_imm_oi_rpc:class_implementer_release(Handle1, ?CONF_CLASS_1),
	    rct_safe_imm_oi_rpc:object_implementer_release(Handle1, 
						       ?CONF_OBJ_2,
						       ?SAFE_IMM_ONE),
	    rct_safe_imm_oi_rpc:object_implementer_release(Handle1, 
						       ?CONF_OBJ_2,
						       ?SAFE_IMM_SUBTREE),
	    rct_safe_imm_oi_rpc:class_implementer_release(Handle1, ?CONF_CLASS_1),
	    ok = rct_safe_imm_oi_rpc:implementer_clear(Handle1),
	    ok = rct_safe_imm_oi_rpc:finalize(Handle1),
	    
	    ct:fail(Reason)
    end.


%%--------------------------------------------------------------------
%% @doc
%% implementer functions multi error. <br/>
%% @spec tc_safe_implfcn_multi_err(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
tc_safe_implfcn_multi_err(_Config) ->
    {ok, Handle1, Vsn1} = rct_safe_imm_oi_rpc:initialize_2(undefined),
    ct:pal("Initialize: safe Handle = ~p, Vsn = ~p", [Handle1, Vsn1]),
    %% Initialize another Handle with no callbacks
    {ok, Handle2, Vsn2} = rct_safe_imm_oi_rpc:initialize_2(undefined),
    ct:pal("Initialize: safe Handle = ~p, Vsn = ~p", [Handle2, Vsn2]),
    try
	%% Set Implementer Name
	ok = rct_safe_imm_oi_rpc:implementer_set(Handle1, ?IMPL_NAME_1),
	
	%% Set class implementer
	ok = rct_safe_imm_oi_rpc:class_implementer_set(Handle1, ?CONF_CLASS_1),
	
	%% Set object implementer with various scope
	ok = rct_safe_imm_oi_rpc:object_implementer_set(Handle1, 
						    ?CONF_OBJ_2,
						    ?SAFE_IMM_ONE),
	ok = rct_safe_imm_oi_rpc:object_implementer_set(Handle1, 
						    ?CONF_OBJ_2,
						    ?SAFE_IMM_SUBLEVEL),
	ok = rct_safe_imm_oi_rpc:object_implementer_set(Handle1, 
						    ?CONF_OBJ_2,
						    ?SAFE_IMM_SUBTREE),

	%% %% Initialize another Handle with no callbacks
	%% {ok, Handle2, Vsn2} = rct_safe_imm_oi_rpc:initialize_2(undefined),
	%% ct:pal("Initialize: safe Handle = ~p, Vsn = ~p", [Handle2, Vsn2]),
	
	%% Try to set an existing implementer name for the new handle
	{error, ?SAFE_AIS_ERR_EXIST} = 
	    rct_safe_imm_oi_rpc:implementer_set(Handle2, ?IMPL_NAME_1),
	
	%% Set a new valid implementer name for the new handle
	ok = rct_safe_imm_oi_rpc:implementer_set(Handle2, ?IMPL_NAME_2),
	
	%% Try to set as class implementer for a class that already has 
	%% another implementer
	{error, ?SAFE_AIS_ERR_EXIST} = 
	    rct_safe_imm_oi_rpc:class_implementer_set(Handle2, ?CONF_CLASS_1),
	
	%% Try to set as object implementer for an object that already has 
	%% another implementer
	{error, ?SAFE_AIS_ERR_EXIST} = 
	    rct_safe_imm_oi_rpc:object_implementer_set(Handle2, 
						   ?CONF_OBJ_2,
						   ?SAFE_IMM_ONE),
	{error, ?SAFE_AIS_ERR_EXIST} = 
	    rct_safe_imm_oi_rpc:object_implementer_set(Handle2, 
						   ?CONF_OBJ_2,
						   ?SAFE_IMM_SUBLEVEL),
	{error, ?SAFE_AIS_ERR_EXIST} = 
	rct_safe_imm_oi_rpc:object_implementer_set(Handle2, 
					       ?CONF_OBJ_2,
					       ?SAFE_IMM_SUBTREE),
	
	%% Release old object implementer scope one and set new implementer
	ok = rct_safe_imm_oi_rpc:object_implementer_release(Handle1, 
							?CONF_OBJ_2,
							?SAFE_IMM_ONE),
	ok = rct_safe_imm_oi_rpc:object_implementer_set(Handle2, 
						    ?CONF_OBJ_2,
						    ?SAFE_IMM_ONE),
	
	%% Try to release other scope for old handle 
	{error, ?SAFE_AIS_ERR_NOT_EXIST} = 
	    rct_safe_imm_oi_rpc:object_implementer_release(Handle1, 
						       ?CONF_OBJ_2,
						       ?SAFE_IMM_SUBLEVEL),
	{error, ?SAFE_AIS_ERR_NOT_EXIST} = 
	    rct_safe_imm_oi_rpc:object_implementer_release(Handle1, 
						       ?CONF_OBJ_2,
						       ?SAFE_IMM_SUBTREE),
	
	%% Try to set with other scope for new handle 
	{error, ?SAFE_AIS_ERR_EXIST} = 
	    rct_safe_imm_oi_rpc:object_implementer_set(Handle2, 
						   ?CONF_OBJ_2,
						   ?SAFE_IMM_SUBLEVEL),
	{error, ?SAFE_AIS_ERR_EXIST} = 
	    rct_safe_imm_oi_rpc:object_implementer_set(Handle2, 
						   ?CONF_OBJ_2,
						   ?SAFE_IMM_SUBTREE),
	
	%% Release new object implementer scope one and set old implementer
	ok = rct_safe_imm_oi_rpc:object_implementer_release(Handle2, 
							?CONF_OBJ_2,
							?SAFE_IMM_ONE),
	ok = rct_safe_imm_oi_rpc:object_implementer_set(Handle1, 
						    ?CONF_OBJ_2,
						    ?SAFE_IMM_ONE),
	
	%% Release object implementer with scope subtree
	ok = rct_safe_imm_oi_rpc:object_implementer_release(Handle1, 
							?CONF_OBJ_2,
							?SAFE_IMM_SUBTREE),
	
	%% Clean up after test
	ok = rct_safe_imm_oi_rpc:class_implementer_release(Handle1, ?CONF_CLASS_1),
	ok = rct_safe_imm_oi_rpc:implementer_clear(Handle1),
	ok = rct_safe_imm_oi_rpc:finalize(Handle1),
	ok = rct_safe_imm_oi_rpc:implementer_clear(Handle2),
	ok = rct_safe_imm_oi_rpc:finalize(Handle2)
    catch
	_:Reason ->
	    rct_safe_imm_oi_rpc:object_implementer_release(Handle1, 
						       ?CONF_OBJ_2,
						       ?SAFE_IMM_ONE),
	    rct_safe_imm_oi_rpc:object_implementer_release(Handle1, 
						       ?CONF_OBJ_2,
						       ?SAFE_IMM_SUBLEVEL),
	    rct_safe_imm_oi_rpc:object_implementer_release(Handle1, 
						       ?CONF_OBJ_2,
						       ?SAFE_IMM_SUBTREE),
	    rct_safe_imm_oi_rpc:object_implementer_release(Handle2, 
						       ?CONF_OBJ_2,
						       ?SAFE_IMM_ONE),
	    rct_safe_imm_oi_rpc:object_implementer_release(Handle1, 
							?CONF_OBJ_2,
						       ?SAFE_IMM_SUBTREE),
	    ok = rct_safe_imm_oi_rpc:class_implementer_release(Handle1, ?CONF_CLASS_1),
	    ok = rct_safe_imm_oi_rpc:implementer_clear(Handle1),
	    ok = rct_safe_imm_oi_rpc:finalize(Handle1),
	    ok = rct_safe_imm_oi_rpc:implementer_clear(Handle2),
	    ok = rct_safe_imm_oi_rpc:finalize(Handle2),
	    
	    ct:fail(Reason)
    end.

%%--------------------------------------------------------------------
%% @doc
%% implmenter function error on runtime class or object. <br/>
%% @spec tc_safe_implfcn_rt_err(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
tc_safe_implfcn_rt_err(_Config) ->
    %% Initialize with no callbacks
    {ok, Handle1, Vsn1} = rct_safe_imm_oi_rpc:initialize_2(undefined),
    ct:pal("Initialize: safe Handle = ~p, Vsn = ~p", [Handle1, Vsn1]),
    try
	%% Set Implementer Name
	ok = rct_safe_imm_oi_rpc:implementer_set(Handle1, ?IMPL_NAME_1),
	
	%% Try to set class implementer for runtime class.
	{error, ?SAFE_AIS_ERR_BAD_OPERATION} = 
	    rct_safe_imm_oi_rpc:class_implementer_set(Handle1, ?RT_CLASS),
	
	%% Create rt object.
	ok = rct_safe_imm_oi_rpc:rt_object_create_2(Handle1,
						?RT_CLASS,
						?CLASS_PARENT,
						[?RT_OBJ_ATTR]),
	%% Try to set object implementer for runtime object
	{error, ?SAFE_AIS_ERR_BAD_OPERATION} = 
	    rct_safe_imm_oi_rpc:object_implementer_set(Handle1, 
						   ?RT_OBJ,
						   ?SAFE_IMM_ONE),
	{error, ?SAFE_AIS_ERR_BAD_OPERATION} = 
	    rct_safe_imm_oi_rpc:object_implementer_set(Handle1, 
						   ?RT_OBJ,
					       ?SAFE_IMM_SUBLEVEL),
	{error, ?SAFE_AIS_ERR_BAD_OPERATION} = 
	    rct_safe_imm_oi_rpc:object_implementer_set(Handle1, 
						   ?RT_OBJ,
						   ?SAFE_IMM_SUBTREE),
	
	%% Clean up after test
	ok = rct_safe_imm_oi_rpc:rt_object_delete(Handle1, ?RT_OBJ),
	ok = rct_safe_imm_oi_rpc:implementer_clear(Handle1),
	ok = rct_safe_imm_oi_rpc:finalize(Handle1)
    catch
	_:Reason ->
	    {ok, TestNode1} = rct_safe_imm_oi_rpc:get_testnode(rct_safe_imm_oi_rpc),
	    rpc:call(TestNode1, safe_imm_oi, rt_object_delete, [Handle1, ?RT_OBJ],  5000),
	    rct_safe_imm_oi_rpc:implementer_clear(Handle1),
	    rct_safe_imm_oi_rpc:finalize(Handle1),
	    
	    ct:fail(Reason)
    end.


%%====================================================================
%% Internal functions
%%====================================================================

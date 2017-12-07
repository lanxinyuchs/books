%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	com_local_authorization_sim_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R7A/1
%%% 
%%% @doc == Test of Custom roles and rules==
%%% <br/><br/>
%%% @end

-module(com_local_authorization_sim_SUITE).
-vsn('/main/R2A/R3A/R7A/1'). 

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%%% R2A/1      2014-04-22 etxjotj     Created
%%% R2A/3      2014-07-11 etxberb     Added mandatory attributes and removed
%%%                                   remove_custom_rule & remove_custom_role.
%%% R3A/1      2015-05-29 etxkols     Changed rct_netconf hook format 
%%% R3A/2      2016-08-19 etxkols     Git migration requires that CC paths is not used 
%%% R7A/1      2016-09-06 etxivri     Changed random to rand due to random is
%%%                                   depricated in OTP19.
%%% ----------------------------------------------------------
%%% 

%compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).

-export([lock/1,
	 unlock/1,
	 create_custom_rule/1,
	 create_custom_role/1,
	 add_custom_rule_in_role/1,
	 change_custom_rule/1]).

-export([create_custom_role2/1,
	 clean/1]).


%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc_1},
		 {rct_netconf,nc},
                 {cth_conn_log,[]},
                 {rct_core,[]},
                 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
					       []}}]}}
		]}].

%% @hidden
init_per_suite(Config) ->
    MeData = rct_rpc:call(rpc_1, comsaI, get_managed_element_data, [], 10000),
    MeId = 
	case proplists:get_value(networkManagedElementId, MeData) of
	    undefined ->
		"1";
	    NMEI -> NMEI
	end,
     [{meId, MeId}|Config].
%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_testcase(TestCase, Config) ->
    ct:print("Now running ~w~n",[TestCase]),
    %% {A,B,C} = os:timestamp(),
    %% ct:log("Seed = ~p~n",[{A,B,C}]),
    %% random:seed(A,B,C),
    Config.
%% @hidden
end_per_testcase(TestCase, Config) ->
    ct:print("Cleaning after ~w~n",[TestCase]),
    clean(Config),
    ct:print("Test case end ~w~n",[TestCase]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> [%% ECIM Use cases
	  unlock, lock, create_custom_rule, create_custom_role,
	  create_custom_role2,
	  add_custom_rule_in_role,
	  change_custom_rule].

%%--------------------------------------------------------------------
%% @doc Unlock local authorization method (UCD 3.3)
%% @end
%%--------------------------------------------------------------------

unlock(Config) ->
    MeId = proplists:get_value(meId, Config),
    Set = {'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],[MeId]},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'SecM', [],
	       [{secMId, ["1"]},
		{'UserManagement',
		 [{userManagementId, ["1"]},
		  {'LocalAuthorizationMethod',
		   [{localAuthorizationMethodId, ["1"]},
		    {administrativeState, ["UNLOCKED"]}]}]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Set]).

%%--------------------------------------------------------------------
%% @doc Lock local authorization method (UCD 3.4)
%% @end
%%--------------------------------------------------------------------

lock(Config) ->
    MeId = proplists:get_value(meId, Config),
    Set = {'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],[MeId]},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'SecM', [],
	       [{secMId, ["1"]},
		{'UserManagement',
		 [{userManagementId, ["1"]},
		  {'LocalAuthorizationMethod',
		   [{localAuthorizationMethodId, ["1"]},
		    {administrativeState, ["LOCKED"]}]}]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Set]).

%%--------------------------------------------------------------------
%% @doc Create a Custom Rule (UCD 3.5)
%% @end
%%--------------------------------------------------------------------

create_custom_rule(Config) -> 
    MeId = proplists:get_value(meId, Config),
    RuleName = proplists:get_value(ruleName, Config, "create_custom_rule"),
    RuleData = proplists:get_value(ruledata, Config, "ManagedElement"),
    Permission = proplists:get_value(permission, Config, "R"),
    Set = {'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],[MeId]},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'SecM', [],
	       [{secMId, ["1"]},
		{'UserManagement',
		 [{userManagementId, ["1"]},
		  {'LocalAuthorizationMethod',
		   [{localAuthorizationMethodId, ["1"]},
		    {'CustomRule', 
		     [{customRuleId, [RuleName]},
		      {permission, [Permission]},
		      {ruleData, [RuleData]},
		      {ruleName, [RuleName]},
		      {userLabel, ["123"]}]}]}]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Set]).

%%--------------------------------------------------------------------
%% @doc Create a Custom Role (UCD 3.6)
%% @end
%%--------------------------------------------------------------------
create_custom_role(Config) -> 
    MeId = proplists:get_value(meId, Config),
    create_custom_rule([{ruleName, "create_custom_role_rule"},
			{ruleData, "MangedElement"},
			{permission, "R"}|Config]),
    RuleDn = 
	"ManagedElement=1,SystemFunctions=1,SecM=1,UserManagement=1,"
	"LocalAuthorizationMethod=1,CustomRule=create_custom_role_rule",
    RoleName = 
	"create_custom_role_"++[$a+rand:uniform(25)||_<-lists:seq(1,10)],
    Set = {'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],[MeId]},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'SecM', [],
	       [{secMId, ["1"]},
		{'UserManagement',
		 [{userManagementId, ["1"]},
		  {'LocalAuthorizationMethod',
		   [{localAuthorizationMethodId, ["1"]},
		    {'CustomRole', 
		     [{customRoleId, [RoleName]},
		      {roleName, [RoleName]},
		      {rules, [RuleDn]},
		      {userLabel, ["123"]}]}]}]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Set]),
    Get = {'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],[MeId]},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'SecM', [],
	       [{secMId, ["1"]},
		{'UserManagement',
		 [{userManagementId, ["1"]},
		  {'LocalAuthorizationMethod',
		   [{localAuthorizationMethodId, ["1"]},
		    {'CustomRule', 
		     [{customRuleId, ["create_custom_role_rule"]}]}]}]}]}]}]},
    {ok, Res} = netconf(get, [nc1, Get]),
    case extract_element(reservedByRoles, Res) of
	{ok, {_, _, ReservedBy}} ->
	    ct:print("ReservedBy = ~p~n",[ReservedBy]);
	not_found ->
	    ct:fail({reservedByRoles, not_found})
    end.


%%--------------------------------------------------------------------
%% @doc Create a Custom Role variant
%% Create a Custom Role, delete it and create it again using same role name
%% @end
%%--------------------------------------------------------------------

create_custom_role2(Config) -> 
    MeId = proplists:get_value(meId, Config),
    create_custom_rule([{ruleName, "create_custom_role_rule"},
			{ruleData, "MangedElement"},
			{permission, "R"}|Config]),
    RuleDn = 
	"ManagedElement=1,SystemFunctions=1,SecM=1,UserManagement=1,"
	"LocalAuthorizationMethod=1,CustomRule=create_custom_role_rule",
    RoleName = [$a+rand:uniform(25)||_<-lists:seq(1,10)],
    ct:print("RoleName = ~p~n",[RoleName]),
    Set = {'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],[MeId]},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'SecM', [],
	       [{secMId, ["1"]},
		{'UserManagement',
		 [{userManagementId, ["1"]},
		  {'LocalAuthorizationMethod',
		   [{localAuthorizationMethodId, ["1"]},
		    {'CustomRole', 
		     [{customRoleId, [RoleName]},
		      {roleName, [RoleName]},
		      {rules, [RuleDn]},
		      {userLabel, ["123"]}]}]}]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Set]),
    clean(Config),
    ok = netconf(edit_config, [nc1, running, Set]).
    

%%--------------------------------------------------------------------
%% @doc Add Custom Rules in a Custom Role (UCD 3.7)
%% Does not work, probably a variant of HS52413
%% @end
%%--------------------------------------------------------------------
add_custom_rule_in_role(Config) -> 
    MeId = proplists:get_value(meId, Config),
    RuleDn = 
	"ManagedElement=1,SystemFunctions=1,SecM=1,UserManagement=1,"
	"LocalAuthorizationMethod=1,CustomRule=com_local_authorization_SUITE",
    RuleDn2 = 
	"ManagedElement=1,SystemFunctions=1,SecM=1,UserManagement=1,"
	"LocalAuthorizationMethod=1,CustomRule=com_local_authorization_SUITE_2",
    Set = {'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],[MeId]},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'SecM', [],
	       [{secMId, ["1"]},
		{'UserManagement',
		 [{userManagementId, ["1"]},
		  {'LocalAuthorizationMethod',
		   [{localAuthorizationMethodId, ["1"]},
		    {'CustomRule', 
		     [{customRuleId, ["com_local_authorization_SUITE_2"]},
		      {permission, ["NO_ACCESS"]},
		      {ruleData, ["SystemFunctions"]},
		      {ruleName, ["com_local_authorization_SUITE_2"]}]},
		    {'CustomRole', 
		     [{customRoleId, ["com_local_authorization_SUITE"]},
		      {rules, [RuleDn, ",", RuleDn2]},
		      {roleName, ["com_local_authorization_SUITE"]}]}]}]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Set]).

%%--------------------------------------------------------------------
%% @doc Change a Custom Rule (UCD 3.8)
%% @end
%%--------------------------------------------------------------------

change_custom_rule(Config) -> 
    MeId = proplists:get_value(meId, Config),
    Set = {'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],[MeId]},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'SecM', [],
	       [{secMId, ["1"]},
		{'UserManagement',
		 [{userManagementId, ["1"]},
		  {'LocalAuthorizationMethod',
		   [{localAuthorizationMethodId, ["1"]},
		    {'CustomRule', 
		     [{customRuleId, ["com_local_authorization_SUITE"]},
		      {userLabel, ["1234"]},
		      {permission, ["RWX"]},
		      {ruleData, ["ManagedElement"]}]}]}]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Set]).

%%--------------------------------------------------------------------
%% @doc Remove all custom configuration
%% @end
%%--------------------------------------------------------------------

clean(Config) ->
    MeId = proplists:get_value(meId, Config),
    Get = {'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"},
	    {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
	   [{managedElementId,[],[MeId]},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'SecM', [],
	       [{secMId, ["1"]},
		{'UserManagement',
		 [{userManagementId, ["1"]},
		  {'LocalAuthorizationMethod',
		   [{localAuthorizationMethodId, ["1"]}]}]}]}]}]},
    Reply= netconf(get_config, [nc1, running, Get]),
    {ok, {_, _, Content}} = 
	extract_element('LocalAuthorizationMethod', [Reply]),
    DeleteObjs = 
	[begin
	     KeyAttr = case Class of
			   'CustomRole' -> customRoleId;
			   'CustomRule' -> customRuleId
		       end,
	     {value, KeyValue} = lists:keysearch(KeyAttr, 1, Attrs),
	     {Class,[{'xc:operation', "delete"}],[KeyValue]}
	 end
	 ||{Class, _, Attrs}<-Content,
	   Class == 'CustomRule' orelse Class == 'CustomRole'],
    case DeleteObjs of
	[] -> ok;
	_ ->
	    Edit = {'ManagedElement',
		    [{xmlns,"urn:com:ericsson:ecim:ComTop"},
		     {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
		    [{managedElementId,[],[MeId]},
		     {'SystemFunctions',
		      [{systemFunctionsId,[],["1"]},
		       {'SecM', [],
			[{secMId, ["1"]},
			 {'UserManagement',
			  [{userManagementId, ["1"]},
			   {'LocalAuthorizationMethod',
			    [{localAuthorizationMethodId, ["1"]}|
			     DeleteObjs]}]}]}]}]},
	    netconf(edit_config, [nc1, running, Edit])
    end.


    
    

%%%--------------------------------------------------------------------
%%% LIBRARY FUNCTIONS
%%%--------------------------------------------------------------------
%%%--------------------------------------------------------------------
%%% Description: Netconf
%%%--------------------------------------------------------------------

netconf(F, A) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    case apply(ct_netconfc, F, A) of
	Res when element(1, Res) == error ->
	    ct:print("Result on ~w:~n~p~n",[F, Res]),
	    Res;
	Res ->
	    case ct_netconfc:close_session(nc1) of
		Res2 when element(1, Res2) == error ->
		    ct:print("Result on close:~n~p~n",[Res2]),
		    ct:fail(Res2);
		_ ->
		    Res
	    end
    end.

%%%--------------------------------------------------------------------
%%% Description: From a general short xml syntax, extract a certain xml
%%%              element
%%%--------------------------------------------------------------------

extract_element(Element, [{Element, Attribute, Content}|_]) ->
    {ok, {Element, Attribute, Content}};
extract_element(Element, [{Element, Content}|_]) ->
    {ok, {Element, Content}};
extract_element(Element, [{_, _, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [{_, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [_|Elements]) ->
    extract_element(Element, Elements);
extract_element(_, []) ->
    not_found.


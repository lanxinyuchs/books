%%% %CCaseFile:	cmsi_c_SUITE.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R9A/R10A/R11A/3

%%% @doc ==Tests of XYZ==
%%% This test suite exercises the CMSI interface.
%%%
%%% To run the dialyzer on this module:
%%%
%%% dialyzer cmsi_c_SUITE.erl $RCT_TOP/test/lib/rct-proxy/esrc/rct_proxy.erl
%%% @end

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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


-module(cmsi_c_SUITE).
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% R2A/7      2013-02-28 etxkols     Added rct_core hook
%%% R2A/8      2013-04-17 etxjovp     change timetrap to 30
%%% R3A/1      2015-07-10 etxjovp     Add group definitions used by CS CI
%%% R4A/5      2015-12-14 etxpeno     support for IPv6 (not on target) in cmsi_get_oap_data/1, cmsi_get_oap_data2/1
%%% R5A/1      2015-12-18 erarafo     Changed random constant to be in 0..2^31-1 range
%%% R5A/2      2016-01-26 etxpeno     Added sleep 1 second in close_trans()
%%% R5A/3      2016-01-27 etxpeno     Only added sleep after close_trans()
%%%                                   when needed
%%% R5A/4      2016-04-22 etxpeno     Moved old test code to new test cases
%%% R7A/1      2016-10-18 etxpeno     add more tests in cmsi_oneshot_positive/1
%%% R9A/1      2017-03-01 etxpeno     Increase sleep time
%%% R11A/3     2017-09-21 etxpeno     add retries in open_trans()
%%% ----------------------------------------------------------

-export([
	 suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 init_per_group/2,
	 end_per_group/2,
	 groups/0,
	 all/0
	]).

-export([
	 cmsi_oneshot_positive/1,
	 cmsi_oneshot_nonexisting_instances_positive/1,
	 cmsi_longlived_positive/1,
	 cmsi_code_example_positive/1,
	 cmsi_massive_test_positive/1,
	 cmsi_two_clients_scenario_positive/1,
	 cmsi_interactive/1
	]).

-export([
	 cmsi_terminate_service_negative/1,
	 %% cmsi_longlived_invalid_handle_negative/1,
	 cmsi_invalid_parameter_direction_negative/1,
	 cmsi_mo_class_not_found_negative/1,
	 cmsi_parent_not_found_negative/1,
	 cmsi_invalid_parameter_todnp_a_negative/1,
	 cmsi_invalid_parameter_todnp_b_negative/1,
	 cmsi_get_network_managed_element_id/1,
	 cmsi_get_managed_element_user_label/1,
	 cmsi_get_oap_data/1,
	 cmsi_get_oap_data2/1,
	 cmsi_get_bidir_ass_for_client/1,
	 cmsi_get_imm_class_name/1,
	 cmsi_is_mo_class_struct/1,
	 cmsi_get_action_name/1,
	 cmsi_get_admop_id/1,
	 cmsi_get_object_id/1,
	 cmsi_get_struct_ref_for_attr/1,
	 cmsi_is_attr_ref2struct/1,
	 cmsi_get_dn_prefix/1
	]).

-export([
	 handleForm/3
	]).



-include_lib("common_test/include/ct.hrl").

-include("test_cmsi.hrl").

-define(NETCNF, nc1).
-define(CLI, c1).
-define(COMTOP, {xmlns, "urn:com:ericsson:ecim:ComTop"}).
-define(NC_NAMESPACE, "urn:ietf:params:xml:ns:netconf:base:1.0").
-define(DELETE, [{'xmlns:nc', ?NC_NAMESPACE}, {'nc:operation', "delete"}]).
-define(SLEEP_AFTER_EDIT, timer:seconds(10)).

%% random constant not exceeding 16#7FFFFFFF (2^31-1).
-define(BAD_OBJ_ID, 16#DEADBEE).

%%% ----------------------------------------------------------
%%% COMMON TEST CALLBACK FUNCTIONS
%%% For descriptions, see the Common Test manual.
%%% ----------------------------------------------------------

-type config() :: [{atom(), term()}].


%% @hidden
-spec suite() -> config().

suite() ->
    [
     {timetrap, {minutes, 30}},
     {ct_hooks, [{rct_htmllink,[]},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
					       []}}]}},
		 {rct_proxy,[{1, node1, ssh_lmt_ipv4, du1, username}]},
		 {rct_core,[]},
                 {rct_netconf,{?NETCNF, html}},
		 {rct_cli, {?CLI,[manual_connect]}}
		]}
    ].


%% @hidden
-spec init_per_suite(config()) -> config().

init_per_suite(Config) ->
    {ok, client_started} = rct_proxy:start_proxy(node1, cmsi1, ?CMSI),
    Config.


%% @hidden
-spec end_per_suite(config()) -> any().

end_per_suite(_Config) ->
    ok.


%% @hidden
-spec init_per_group(atom(), config()) -> config().

init_per_group(_GroupName, Config) ->
    Config.


%% @hidden
-spec end_per_group(atom(), config()) -> any().

end_per_group(_GroupName, _Config) ->
    ok.


%% @hidden
-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
    Config.


%% @hidden
-spec end_per_testcase(atom(), config()) -> any().

end_per_testcase(_TestCase, _Config) ->
    {ok} = send(?CLIENT_0, ?CMSI_CLEANUP, ?IGNORED_TRANSPORT,
		?IGNORED_DIRECTION, ""),
    ok.


%% @hidden
-spec groups() -> [{atom(), list(), list()}].

groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},
     {sbc__upgrade_short__all__1__group, [], [{group, default__group}]},
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},
     {sdc__qual__all__1__group, [], []},
     %% This interface should only be supported on core MP within a sim cluster
     {sbc__cluster__dual_sim_1__1__group, [], [{group, default__group}]}
    ].


%% @hidden
-spec all() -> list() | {skip, term()}.

all() ->
    [
     cmsi_oneshot_positive,
     cmsi_oneshot_nonexisting_instances_positive,
     cmsi_longlived_positive,
     cmsi_code_example_positive,
     cmsi_massive_test_positive,
     cmsi_two_clients_scenario_positive,
     cmsi_terminate_service_negative,
     %% cmsi_longlived_invalid_handle_negative,
     cmsi_invalid_parameter_direction_negative,
     cmsi_mo_class_not_found_negative,
     cmsi_parent_not_found_negative,
     cmsi_invalid_parameter_todnp_a_negative,
     cmsi_invalid_parameter_todnp_b_negative,
     cmsi_get_network_managed_element_id,
     cmsi_get_managed_element_user_label,
     cmsi_get_oap_data,
     cmsi_get_oap_data2,
     cmsi_get_bidir_ass_for_client,
     cmsi_get_imm_class_name,
     cmsi_is_mo_class_struct,
     cmsi_get_action_name,
     cmsi_get_admop_id,
     cmsi_get_object_id,
     cmsi_get_struct_ref_for_attr,
     cmsi_is_attr_ref2struct,
     cmsi_get_dn_prefix
    ].


%%% ----------------------------------------------------------
%%% TEST CASES
%%% ----------------------------------------------------------
cmsi_oneshot_positive(_Config) ->

    {ok, ?CMSI_OK, "testClass1Id=1,TESTMOMtestRootId=1"} =
	toImm(?CLIENT_0, ?ONESHOT, "ManagedElement=1,TestRoot=1,TestClass1=1"),

    {ok, ?CMSI_OK, "ManagedElement=1,TestRoot=1,TestClass1=1"} =
	to3gpp(?CLIENT_0, ?ONESHOT, "testClass1Id=1,TESTMOMtestRootId=1"),

    {ok, ?CMSI_OK, "testClass3Id=1,testClass2Id=1,TESTMOMtestRootId=1"} =
	toImm(?CLIENT_0, ?ONESHOT,
	      "ManagedElement=1,TestRoot=1,TestClass2=1,TestClass3=1"),

    {ok, ?CMSI_OK, "ManagedElement=1,TestRoot=1,TestClass2=1,TestClass3=1"} =
	to3gpp(?CLIENT_0, ?ONESHOT,
	       "testClass3Id=1,testClass2Id=1,TESTMOMtestRootId=1"),

    {ok, ?CMSI_OK, "oamAccessPointId=1,sysMId=1,systemFunctionsId=1"} =
	toImm(?CLIENT_0, ?ONESHOT,
	      "ManagedElement=1,SystemFunctions=1,SysM=1,OamAccessPoint=1"),

    {ok, ?CMSI_OK,
     "ManagedElement=1,SystemFunctions=1,SysM=1,OamAccessPoint=1"} =
	to3gpp(?CLIENT_0, ?ONESHOT,
	       "oamAccessPointId=1,sysMId=1,systemFunctionsId=1"),

    {ok, ?CMSI_OK, "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1"} =
	to3gpp(?CLIENT_0, ?ONESHOT,
	      "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1"),

     {ok, ?CMSI_OK, "ManagedElement=1,NodeSupport=1,ServiceDiscovery=1"} =
	to3gpp(?CLIENT_0, ?ONESHOT,
	      "ManagedElement=1,NodeSupport=1,ServiceDiscovery=1"),

    {ok, ?CMSI_OK, ""} = toImm(?CLIENT_0, ?ONESHOT, "ManagedElement=1"),

    {ok, ?CMSI_OK, ""} = toImm(?CLIENT_0, ?ONESHOT,
			       "ManagedElement=1,SystemFunctions=1"),

    {ok, ?CMSI_OK, ""} = toImm(?CLIENT_0, ?ONESHOT,
			       "ManagedElement=1,SystemFunctions=1,SysM=1"),

    {ok, ?CMSI_OK, "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1"} =
	toImm(?CLIENT_0, ?ONESHOT,
	      "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1"),

     {ok, ?CMSI_OK, "ManagedElement=1,NodeSupport=1,ServiceDiscovery=1"} =
	toImm(?CLIENT_0, ?ONESHOT,
	      "ManagedElement=1,NodeSupport=1,ServiceDiscovery=1"),

   {ok, ?CMSI_OK,
     "ribbingId=1,flemingId=1,ribbingId=1,flemingId=1,NOBLEheliumId=1"} =
	toImm(?CLIENT_0, ?ONESHOT,
	      "ManagedElement=1,Helium=1,Fleming=1,Ribbing=1,Fleming=1,Ribbing=1"),

    {ok, ?CMSI_OK,
     "ManagedElement=1,Helium=1,Fleming=1,Ribbing=1,Fleming=1,Ribbing=1"} =
	to3gpp(?CLIENT_0, ?ONESHOT,
	       "ribbingId=1,flemingId=1,ribbingId=1,flemingId=1,NOBLEheliumId=1"),

    ok.


%% @doc Transform DNs with RND value parts not present in the current MO tree.

cmsi_oneshot_nonexisting_instances_positive(_Config) ->
    {ok, ?CMSI_OK, "testClass1Id=carbon,TESTMOMtestRootId=ALPHA"} =
	toImm(?CLIENT_0, ?ONESHOT,
	      "ManagedElement=17,TestRoot=ALPHA,TestClass1=carbon"),
    {ok, ?CMSI_OK, "ManagedElement=1,TestRoot=ALPHA,TestClass1=carbon"} =
	to3gpp(?CLIENT_0, ?ONESHOT,
	       "testClass1Id=carbon,TESTMOMtestRootId=ALPHA"),
    ok.


%% @doc Multiple transformations, using long-lived connection, both ways.

cmsi_longlived_positive(_Config) ->
    {ok} =
	initiateService(?CLIENT_1),

    {ok, ?CMSI_OK, "testClass1Id=1,TESTMOMtestRootId=1"} =
	toImm(?CLIENT_1, ?LONGLIVING, "ManagedElement=1,TestRoot=1,TestClass1=1"),

    {ok, ?CMSI_OK, "ManagedElement=1,TestRoot=1,TestClass1=1"} =
	to3gpp(?CLIENT_1, ?LONGLIVING, "testClass1Id=1,TESTMOMtestRootId=1"),

    {ok} = terminateService(?CLIENT_1),

    ok.


%% @doc Exercise the code example from the IWD.

cmsi_code_example_positive(_Config) ->
    {ok, ?CMSI_OK} =
	send(
	  ?CLIENT_0,
	  ?CMSI_CODE_EXAMPLE,
	  ?IGNORED_TRANSPORT,
	  ?MIM_TO_IMM, "ManagedElement=1,TestRoot=1,TestClass1=1"),
    ok.


%% @doc Perform multiple transformations. It seems that each transformation
%% takes of the order 50 ms in the simulated environment on a VDI.

cmsi_massive_test_positive(_Config) ->
    {ok} = initiateService(?CLIENT_1),
    repeatSend(?REPEAT,
	       ?CLIENT_1,
	       ?MIM_TO_IMM,
	       "ManagedElement=1,TestRoot=1,TestClass1=1",
	       "testClass1Id=1,TESTMOMtestRootId=1"),
    {ok} = terminateService(?CLIENT_1),

    {ok} = initiateService(?CLIENT_1),
    repeatSend(?REPEAT,
	       ?CLIENT_1,
	       ?IMM_TO_MIM,
	       "testClass1Id=1,TESTMOMtestRootId=1",
	       "ManagedElement=1,TestRoot=1,TestClass1=1"),
    {ok} = terminateService(?CLIENT_1),

    {ok} = initiateService(?CLIENT_1),
    ok = repeatIsMoClassStruct(?CLIENT_1, ?LONGLIVING, "TESTMOMTestClass4",
			       {ok, ?CMSI_OK, false}, ?REPEAT),
    {ok} = terminateService(?CLIENT_1),

    {ok} = initiateService(?CLIENT_1),
    ok = repeatIsAttrRef2Struct(?CLIENT_1, ?LONGLIVING, "TESTMOMTestClass3",
				"managedElement", {ok, ?CMSI_OK, false},
				?REPEAT),
    {ok} = terminateService(?CLIENT_1),

    {ok} = initiateService(?CLIENT_1),
    ok = repeatGetStructRefForAttr(?CLIENT_1, ?LONGLIVING, "TESTMOMTestClass3",
    				   "struct3seq",
    				   {ok, ?CMSI_OK, "TESTMOMStruct3"}, ?REPEAT),
    {ok} = terminateService(?CLIENT_1),

    ok.


%% @doc Scenario where two clients are active at the same time.

cmsi_two_clients_scenario_positive(_Config) ->
    {ok} =
	initiateService(?CLIENT_1),

    {ok, ?CMSI_OK, "testClass1Id=1,TESTMOMtestRootId=1"} =
	toImm(?CLIENT_1, ?LONGLIVING,
	      "ManagedElement=1,TestRoot=1,TestClass1=1"),
    {ok, ?CMSI_OK, true} = isMoClassStruct(?CLIENT_1, ?LONGLIVING,
					   "TESTMOMStruct3"),
    {ok, ?CMSI_OK, true} = isAttrRef2Struct(?CLIENT_1, ?LONGLIVING,
					    "TESTMOMTestClass3", "struct3seq"),
    {ok, ?CMSI_OK, "TESTMOMStruct3"} = getStructRefForAttr(?CLIENT_1,
    							   ?LONGLIVING,
    							   "TESTMOMTestClass3",
    							   "struct3seq"),

    {ok} =
	initiateService(?CLIENT_2),

    {ok, ?CMSI_OK, "testClass1Id=1,TESTMOMtestRootId=1"} =
	toImm(?CLIENT_1, ?LONGLIVING, "ManagedElement=1,TestRoot=1,TestClass1=1"),
    {ok, ?CMSI_OK, true} = isMoClassStruct(?CLIENT_1, ?LONGLIVING,
					   "TESTMOMStruct3"),
    {ok, ?CMSI_OK, true} = isAttrRef2Struct(?CLIENT_1, ?LONGLIVING,
					    "TESTMOMTestClass3", "struct3seq"),
    {ok, ?CMSI_OK, "TESTMOMStruct3"} = getStructRefForAttr(?CLIENT_1,
    							   ?LONGLIVING,
    							   "TESTMOMTestClass3",
    							   "struct3seq"),

    {ok, ?CMSI_OK, "testClass1Id=1,TESTMOMtestRootId=1"} =
	toImm(?CLIENT_2, ?LONGLIVING, "ManagedElement=1,TestRoot=1,TestClass1=1"),
    {ok, ?CMSI_OK, true} = isMoClassStruct(?CLIENT_2, ?LONGLIVING,
					   "TESTMOMStruct3"),
    {ok, ?CMSI_OK, true} = isAttrRef2Struct(?CLIENT_2, ?LONGLIVING,
					    "TESTMOMTestClass3", "struct3seq"),
    {ok, ?CMSI_OK, "TESTMOMStruct3"} = getStructRefForAttr(?CLIENT_2,
    							   ?LONGLIVING,
    							   "TESTMOMTestClass3",
    							   "struct3seq"),

    {ok} = terminateService(?CLIENT_1),

    {ok, ?CMSI_OK, "testClass1Id=1,TESTMOMtestRootId=1"} =
	toImm(?CLIENT_2, ?LONGLIVING, "ManagedElement=1,TestRoot=1,TestClass1=1"),
    {ok, ?CMSI_OK, true} = isMoClassStruct(?CLIENT_2, ?LONGLIVING,
					   "TESTMOMStruct3"),
    {ok, ?CMSI_OK, true} = isAttrRef2Struct(?CLIENT_2, ?LONGLIVING,
					    "TESTMOMTestClass3", "struct3seq"),
    {ok, ?CMSI_OK, "TESTMOMStruct3"} = getStructRefForAttr(?CLIENT_2,
    							   ?LONGLIVING,
    							   "TESTMOMTestClass3",
    							   "struct3seq"),

    {ok} = terminateService(?CLIENT_2),

    ok.



%% @doc Interactive test. Use a web browser to interact with the test case.
%% Look in the console output for the URL to be opened.

cmsi_interactive(_Config) ->
    ct:timetrap({minutes, 100}),
    inets:start(),
    ct:print("service info: ~p", [inets:services_info()]),
    ServerPid = startHttpd(),
    register(cmsi_interactive, self()),
    receive
	X ->
	    ct:print("test case received: ~p, finishing", [X])
    end,
    inets:stop(httpd, ServerPid),
    inets:stop(),
    ok.




%%% ----------------------------------------------------------
%%% NEGATIVE TEST CASES
%%% ----------------------------------------------------------

%% @doc This test just verifies behaviour of test_cmsi.c;
%% the CMSI interface is not invoked.

cmsi_terminate_service_negative(_Config) ->
    {error, "no active connection for client", ?CLIENT_1} =
	terminateService(?CLIENT_1),
    ok.


%% This test is actually not safe to execute for now. It will cause the
%% CMSI interface to try and send data on a random-numbered socket. The
%% operation may result in an error return (which is a good behaviour), or
%% a thread crash (which has also been observed).
%%
%% %% @doc Verify error handling: Trying to use a handle after it was terminated.
%%
%% cmsi_longlived_invalid_handle_negative(_Config) ->
%%     {ok} =
%% 	initiateService(?CLIENT_1),
%%
%%     {ok, ?CMSI_OK, "testClass1Id=1,TESTMOMtestRootId=1"} =
%% 	toImm(?CLIENT_1, ?LONGLIVING, "ManagedElement=1,TestRoot=1,TestClass1=1"),
%%
%%     {ok} =
%% 	send(?CLIENT_1, ?CMSI_TERMINATE_SERVICE_KEEP_HANDLE, ?IGNORED_TRANSPORT, ?IGNORED_DIRECTION, ?IGNORED_DN),
%%
%%     {ok, ?CMSI_SEND_ERROR, "CMSI_SEND_ERROR"} =
%% 	toImm(?CLIENT_1, ?LONGLIVING, "ManagedElement=1,TestRoot=1,TestClass1=1"),
%%
%%     ok.


%% @doc Verify error handling: Invalid transformation direction.

cmsi_invalid_parameter_direction_negative(_Config) ->
    InvalidDirection = 17,
    {ok, ?CMSI_INVALID_PARAMETER_DIRECTION, "CMSI_INVALID_PARAMETER_DIRECTION"} =
	send(?CLIENT_0, ?CMSI_TRANSFORM, ?ONESHOT, InvalidDirection, "ManagedElement=1,XyzFunction=1"),
    ok.


%% @doc Verify error handling: MO class not found.

cmsi_mo_class_not_found_negative(_Config) ->
    {ok, ?CMSI_OBJECT_CLASS_NOT_FOUND, "XyzFunction"} =
	toImm(?CLIENT_0, ?ONESHOT, "ManagedElement=1,XyzFunction=1"),

    {ok, ?CMSI_OBJECT_CLASS_NOT_FOUND, "ManagedThing"} =
	toImm(?CLIENT_0, ?ONESHOT, "ManagedThing=1,XyzFunction=1"),

    %% TR HU55442
    {ok, ?CMSI_OBJECT_CLASS_NOT_FOUND, "ManagedElement"} =
     	toImm(?CLIENT_0, ?ONESHOT, "ManagedElement=1,XyzFunction"),

    {ok, ?CMSI_OBJECT_CLASS_NOT_FOUND, "nosuchClassId"} =
	to3gpp(?CLIENT_0, ?ONESHOT, "nosuchClassId=33"),

    {ok, ?CMSI_OBJECT_CLASS_NOT_FOUND, "testClass99Id"} =
	to3gpp(?CLIENT_0, ?ONESHOT, "testClass3Id=1,testClass99Id=1,TESTMOMtestRootId=1"),

    {ok, ?CMSI_OBJECT_CLASS_NOT_FOUND, "TESTMOMnosuchTestRootId"} =
	to3gpp(?CLIENT_0, ?ONESHOT, "testClass3Id=1,testClass2Id=1,TESTMOMnosuchTestRootId=1"),

    {ok, ?CMSI_OBJECT_CLASS_NOT_FOUND, "MISSINGMOMtestRootId"} =
	to3gpp(?CLIENT_0, ?ONESHOT, "testClass3Id=1,testClass2Id=1,MISSINGMOMtestRootId=1"),

    ok.

cmsi_parent_not_found_negative(_Config) ->
    {ok, ?CMSI_OBJECT_CLASS_NOT_FOUND, "TestClass3"} =
	toImm(?CLIENT_0, ?ONESHOT, "ManagedElement=1,TestClass3=1").

%% @doc Verify error handling: Invoking cmsiTransform with an incorrect
%% output parameter: the location specified by the 4th argument should
%% contain NULL but instead contains a non-NULL pointer.

cmsi_invalid_parameter_todnp_a_negative(_Config) ->

    {ok, ?CMSI_INVALID_PARAMETER_TODNP, "CMSI_INVALID_PARAMETER_TODNP"} =
	send(?CLIENT_0, ?CMSI_PASS_INVALID_TODNP_A, ?ONESHOT, ?MIM_TO_IMM, "X=1,Y=1"),

    ok.


%% @doc Verify error handling: Invoking cmsiTransform with the 4th argument
%% set to NULL.

-spec cmsi_invalid_parameter_todnp_b_negative(config()) -> ok.

cmsi_invalid_parameter_todnp_b_negative(_Config) ->

    {ok, ?CMSI_INVALID_PARAMETER_TODNP, "CMSI_INVALID_PARAMETER_TODNP"} =
	send(?CLIENT_0, ?CMSI_PASS_INVALID_TODNP_B, ?ONESHOT, ?MIM_TO_IMM, "ManagedElement=1,TestRoot=1,TestClass1=1"),

    ok.

-spec cmsi_get_network_managed_element_id(config()) -> ok.
cmsi_get_network_managed_element_id(_Config) ->
    OrigNmeId = "1",
    OrigNmeIdLen = length(OrigNmeId),
    NewNmeId = "test of new networkManagedElementId",
    NewNmeIdLen = length(NewNmeId),

    LongSize = 100,
    ShortSize = 10,
    TruncNmeId = lists:sublist(NewNmeId, ShortSize-1),

    {ok} = initiateService(?CLIENT_1),

    {ok, ?CMSI_OK, OrigNmeId, OrigNmeIdLen} =
	getNmei(?CLIENT_0, ?ONESHOT, LongSize),
    {ok, ?CMSI_OK, OrigNmeId, OrigNmeIdLen} =
	getNmei(?CLIENT_1, ?LONGLIVING, LongSize),

    ok = set_networkManagedElementId(OrigNmeId, NewNmeId),

    {ok, ?CMSI_OK, NewNmeId, NewNmeIdLen} =
	getNmei(?CLIENT_0, ?ONESHOT, LongSize),
    {ok, ?CMSI_OK, NewNmeId, NewNmeIdLen} =
	getNmei(?CLIENT_1, ?LONGLIVING, LongSize),

    %% Check truncation
    {ok, ?CMSI_OK, TruncNmeId, NewNmeIdLen} =
	getNmei(?CLIENT_0, ?ONESHOT, ShortSize),
    {ok, ?CMSI_OK, TruncNmeId, NewNmeIdLen} =
	getNmei(?CLIENT_1, ?LONGLIVING, ShortSize),

    ok = set_networkManagedElementId(NewNmeId, OrigNmeId),

    {ok, ?CMSI_OK, OrigNmeId, OrigNmeIdLen} =
	getNmei(?CLIENT_0, ?ONESHOT, LongSize),
    {ok, ?CMSI_OK, OrigNmeId, OrigNmeIdLen} =
	getNmei(?CLIENT_1, ?LONGLIVING, LongSize),

    {ok} = terminateService(?CLIENT_1),

    ok.

-spec cmsi_get_managed_element_user_label(config()) -> ok.
cmsi_get_managed_element_user_label(_Config) ->
    NewUserLabel = "test of new userlabel",
    NewUserLabelLen = length(NewUserLabel),

    LongSize = 100,
    ShortSize = 10,
    TruncUserLabel = lists:sublist(NewUserLabel, ShortSize-1),

    {ok} = initiateService(?CLIENT_1),

    OrigUserLabel = get_managedElementUserLabel(),

    ok = set_managedElementUserLabel(NewUserLabel),

    {ok, ?CMSI_OK, NewUserLabel, NewUserLabelLen} =
	getMeUl(?CLIENT_0, ?ONESHOT, LongSize),
    {ok, ?CMSI_OK, NewUserLabel, NewUserLabelLen} =
	getMeUl(?CLIENT_1, ?LONGLIVING, LongSize),

    %% Check truncation
    {ok, ?CMSI_OK, TruncUserLabel, NewUserLabelLen} =
	getMeUl(?CLIENT_0, ?ONESHOT, ShortSize),
    {ok, ?CMSI_OK, TruncUserLabel, NewUserLabelLen} =
	getMeUl(?CLIENT_1, ?LONGLIVING, ShortSize),

    ok = set_managedElementUserLabel(OrigUserLabel),

    {ok} = terminateService(?CLIENT_1),

    ok.

-spec cmsi_get_oap_data(config()) -> ok.
cmsi_get_oap_data(_Config) ->
    ResetOap = get_OamAccessPoint(),
    DSCP = get_dscp(ResetOap),

    L = create_ip_instances(),

    {ok} = initiateService(?CLIENT_1),

    lists:foreach(
      fun({Type, IP, Ldn}) ->
	      IPNum = get_ipv4_32bits(Type, IP),
	      Oap = get_OamAccessPoint(Ldn),
	      ok = set_OamAccessPoint(Oap, 60),

	      {ok, ?CMSI_OK, _, IPNum, DSCP} = getOapData(?CLIENT_0, ?ONESHOT),
	      {ok, ?CMSI_OK, _, IPNum, DSCP} =
		  getOapData(?CLIENT_1, ?LONGLIVING)
      end, L),

    {ok} = terminateService(?CLIENT_1),

    ok = set_OamAccessPoint(ResetOap, 60),

    ok.

-spec cmsi_get_oap_data2(config()) -> ok.
cmsi_get_oap_data2(_Config) ->
    ResetOap = get_OamAccessPoint(),
    DSCP = get_dscp(ResetOap),

    L = create_ip_instances(),

    {ok} = initiateService(?CLIENT_1),

    lists:foreach(
      fun({Type, IP, Ldn}) ->
	      Oap = get_OamAccessPoint(Ldn),
	      ok = set_OamAccessPoint(Oap, 60),

	      {ok, ?CMSI_OK, _, Type, IP, DSCP} =
		  getOapData2(?CLIENT_0, ?ONESHOT),
	      {ok, ?CMSI_OK, _, Type, IP, DSCP} =
		  getOapData2(?CLIENT_1, ?LONGLIVING)
      end, L),

    {ok} = terminateService(?CLIENT_1),

    ok = set_OamAccessPoint(ResetOap, 60),

    ok.

-spec cmsi_get_bidir_ass_for_client(config()) -> ok.
cmsi_get_bidir_ass_for_client(_Config) ->
    ResultTESTMOMTestClass1 = [],

    ResultTESTMOMTestClassB =
	[{
	   {reserving, [{name,           "uses"},
			{className,      "TestClassD"},
			{mimName,        "TESTMOM"},
			{isScoped,       true},
			{isReserving,    true},
			{minCardinality, 0},
			{maxCardinality, undefined}]
	   },
	   {reserved, [{name,           "reservedBy"},
		       {className,      "TestClassB"},
		       {mimName,        "TESTMOM"},
		       {isScoped,       true},
		       {isReserving,    false},
		       {minCardinality, 0},
		       {maxCardinality, undefined}]
	   }
	 }],

    {ok, ?CMSI_CLASS_NOT_FOUND} =
	getBidirAss(?CLIENT_0, ?ONESHOT, "notAClass"),
    {ok, ?CMSI_OK, ResultTESTMOMTestClass1} =
	getBidirAss(?CLIENT_0, ?ONESHOT, "TESTMOMTestClass1"),
    {ok, ?CMSI_OK, ResultTESTMOMTestClassB} =
	getBidirAss(?CLIENT_0, ?ONESHOT, "TESTMOMTestClassB"),

    {ok} = initiateService(?CLIENT_1),
    {ok, ?CMSI_CLASS_NOT_FOUND} =
	getBidirAss(?CLIENT_1, ?LONGLIVING, "notAClass"),
    {ok, ?CMSI_OK, ResultTESTMOMTestClass1} =
	getBidirAss(?CLIENT_1, ?LONGLIVING, "TESTMOMTestClass1"),
    {ok, ?CMSI_OK, ResultTESTMOMTestClassB} =
	getBidirAss(?CLIENT_1, ?LONGLIVING, "TESTMOMTestClassB"),
    {ok} = terminateService(?CLIENT_1),

    ok.

-spec cmsi_get_imm_class_name(config()) -> ok.
cmsi_get_imm_class_name(_Config) ->
    {ok, ?CMSI_OK, "TESTMOMTestClass1"} =
	getImmClassName(?CLIENT_0,
			?ONESHOT,
			"testClass1Id=1,TESTMOMtestRootId=1"),

    {ok, ?CMSI_OK, "TESTMOMTestClass3"} =
	getImmClassName(?CLIENT_0,
			?ONESHOT,
			"testClass3Id=1,testClass2Id=1,TESTMOMtestRootId=1"),

    {ok, ?CMSI_OK, "OamAccessPoint"} =
	getImmClassName(?CLIENT_0,
			?ONESHOT,
			"oamAccessPointId=1,sysMId=1,systemFunctionsId=1"),

    {ok, ?CMSI_OK, "TESTMOMStruct2"} =
	getImmClassName(?CLIENT_0,
			?ONESHOT,
			"id=struct2,testClass2Id=1,TESTMOMtestRootId=1"),

    {ok, ?CMSI_OK, "TESTMOMStruct3"} =
	getImmClassName(?CLIENT_0,
			?ONESHOT,
			"id=struct3seq_1,testClass3Id=1,testClass2Id=1,TESTMOMtestRootId=1"),

    {ok, ?CMSI_OBJECT_CLASS_NOT_FOUND} =
	getImmClassName(?CLIENT_0, ?ONESHOT, "nosuchClassId=33"),

    {ok, ?CMSI_OBJECT_CLASS_NOT_FOUND} =
	getImmClassName(?CLIENT_0, ?ONESHOT,
			"testClass3Id=1,testClass99Id=1,TESTMOMtestRootId=1"),

    {ok, ?CMSI_OBJECT_CLASS_NOT_FOUND} =
	getImmClassName(?CLIENT_0, ?ONESHOT,
			"testClass3Id=1,testClass2Id=1,TESTMOMnosuchTestRootId=1"),

    {ok, ?CMSI_OBJECT_CLASS_NOT_FOUND} =
	getImmClassName(?CLIENT_0, ?ONESHOT,
			"testClass3Id=1,testClass2Id=1,MISSINGMOMtestRootId=1"),

    {ok} = initiateService(?CLIENT_1),

    {ok, ?CMSI_OK, "TESTMOMTestClass1"} =
	getImmClassName(?CLIENT_1,
			?LONGLIVING,
			"testClass1Id=1,TESTMOMtestRootId=1"),

    {ok, ?CMSI_OK, "TESTMOMTestClass3"} =
	getImmClassName(?CLIENT_1,
			?LONGLIVING,
			"testClass3Id=1,testClass2Id=1,TESTMOMtestRootId=1"),

    {ok, ?CMSI_OK, "OamAccessPoint"} =
	getImmClassName(?CLIENT_1,
			?LONGLIVING,
			"oamAccessPointId=1,sysMId=1,systemFunctionsId=1"),

    {ok, ?CMSI_OK, "TESTMOMStruct2"} =
	getImmClassName(?CLIENT_1,
			?LONGLIVING,
			"id=struct2,testClass2Id=1,TESTMOMtestRootId=1"),

    {ok, ?CMSI_OK, "TESTMOMStruct3"} =
	getImmClassName(?CLIENT_1,
			?LONGLIVING,
			"id=struct3seq_1,testClass3Id=1,testClass2Id=1,TESTMOMtestRootId=1"),

    {ok, ?CMSI_OBJECT_CLASS_NOT_FOUND} =
	getImmClassName(?CLIENT_1, ?LONGLIVING, "nosuchClassId=33"),

    {ok, ?CMSI_OBJECT_CLASS_NOT_FOUND} =
	getImmClassName(?CLIENT_1, ?LONGLIVING,
			"testClass3Id=1,testClass99Id=1,TESTMOMtestRootId=1"),

    {ok, ?CMSI_OBJECT_CLASS_NOT_FOUND} =
	getImmClassName(?CLIENT_1, ?LONGLIVING,
			"testClass3Id=1,testClass2Id=1,TESTMOMnosuchTestRootId=1"),

    {ok, ?CMSI_OBJECT_CLASS_NOT_FOUND} =
	getImmClassName(?CLIENT_1, ?LONGLIVING,
			"testClass3Id=1,testClass2Id=1,MISSINGMOMtestRootId=1"),

    {ok} = terminateService(?CLIENT_1),

    ok.

-spec cmsi_is_mo_class_struct(config()) -> ok.
cmsi_is_mo_class_struct(_Config) ->
    {ok, ?CMSI_OK, false} = isMoClassStruct(?CLIENT_0, ?ONESHOT,
					    "TESTMOMTestClass4"),
    {ok, ?CMSI_OK, true} = isMoClassStruct(?CLIENT_0, ?ONESHOT,
					   "TESTMOMStruct3"),

    {ok, ?CMSI_CLASS_NOT_FOUND, _} =
	isMoClassStruct(?CLIENT_0, ?ONESHOT, "notAClass"),

    {ok} = initiateService(?CLIENT_1),

    {ok, ?CMSI_OK, false} = isMoClassStruct(?CLIENT_1, ?LONGLIVING,
					    "TESTMOMTestClass4"),
    {ok, ?CMSI_OK, true} = isMoClassStruct(?CLIENT_1, ?LONGLIVING,
					   "TESTMOMStruct3"),

    {ok, ?CMSI_CLASS_NOT_FOUND, _} =
	isMoClassStruct(?CLIENT_1, ?LONGLIVING, "notAClass"),

    {ok} = terminateService(?CLIENT_1),

    ok.

-spec cmsi_get_action_name(config()) -> ok.
cmsi_get_action_name(_Config) ->
    {ok, ?CMSI_OK, "test1"} =
	getActionName(?CLIENT_0, ?ONESHOT, "TESTMOMTestClass3", 7),

    {ok, ?CMSI_CLASS_NOT_FOUND} =
	getActionName(?CLIENT_0, ?ONESHOT, "notAClass", 7),

    {ok, ?CMSI_ACTION_NOT_FOUND} =
	getActionName(?CLIENT_0, ?ONESHOT, "TESTMOMStruct3", 7),

    {ok, ?CMSI_ACTION_NOT_FOUND} =
	getActionName(?CLIENT_0, ?ONESHOT, "TESTMOMTestClass3", 999),

    {ok} = initiateService(?CLIENT_1),

    {ok, ?CMSI_OK, "test1"} =
	getActionName(?CLIENT_1, ?LONGLIVING, "TESTMOMTestClass3", 7),

    {ok, ?CMSI_CLASS_NOT_FOUND} =
	getActionName(?CLIENT_1, ?LONGLIVING, "notAClass", 7),

    {ok, ?CMSI_ACTION_NOT_FOUND} =
	getActionName(?CLIENT_1, ?LONGLIVING, "TESTMOMStruct3", 7),

    {ok, ?CMSI_ACTION_NOT_FOUND} =
	getActionName(?CLIENT_1, ?LONGLIVING, "TESTMOMTestClass3", 999),

    {ok} = terminateService(?CLIENT_1),

    ok.

-spec cmsi_get_admop_id(config()) -> ok.
cmsi_get_admop_id(_Config) ->
    {ok, ?CMSI_OK, 7} =
	getAdmOpId(?CLIENT_0, ?ONESHOT, "TESTMOMTestClass3", "test1"),

    {ok, ?CMSI_CLASS_NOT_FOUND} =
	getAdmOpId(?CLIENT_0, ?ONESHOT, "notAClass", "notAnAction"),

    {ok, ?CMSI_ACTION_NOT_FOUND} =
	getAdmOpId(?CLIENT_0, ?ONESHOT, "TESTMOMStruct3", "notAnAction"),

    {ok, ?CMSI_ACTION_NOT_FOUND} =
	getAdmOpId(?CLIENT_0, ?ONESHOT, "TESTMOMTestClass3", "notAnAction"),

    {ok} = initiateService(?CLIENT_1),

    {ok, ?CMSI_OK, 7} =
	getAdmOpId(?CLIENT_1, ?LONGLIVING, "TESTMOMTestClass3", "test1"),

    {ok, ?CMSI_CLASS_NOT_FOUND} =
	getAdmOpId(?CLIENT_1, ?LONGLIVING, "notAClass", "notAnAction"),

    {ok, ?CMSI_ACTION_NOT_FOUND} =
	getAdmOpId(?CLIENT_1, ?LONGLIVING, "TESTMOMStruct3", "notAnAction"),

    {ok, ?CMSI_ACTION_NOT_FOUND} =
	getAdmOpId(?CLIENT_1, ?LONGLIVING, "TESTMOMTestClass3", "notAnAction"),

    {ok} = terminateService(?CLIENT_1),

    ok.

-spec cmsi_get_object_id(config()) -> ok.
cmsi_get_object_id(_Config) ->
    {ok, ?CMSI_OK, ObjectId} =
	getObjectId(?CLIENT_0, ?ONESHOT,
		    "ManagedElement=1,TestRoot=1,TestClass1=1"),
    {ok, ?CMSI_OK, ObjectId} =
	getObjectId(?CLIENT_0, ?ONESHOT,
		    "ManagedElement=1,TestRoot=1,TestClass1=1"),
    {ok, ?CMSI_OK, ObjectId2} =
	getObjectId(?CLIENT_0, ?ONESHOT,
		    "ManagedElement=1,TestRoot=1,TestClass1=2"),
    {ok, ?CMSI_OK, "ManagedElement=1,TestRoot=1,TestClass1=1"} =
	getLdn(?CLIENT_0, ?ONESHOT, ObjectId),
    {ok, ?CMSI_OK, "ManagedElement=1,TestRoot=1,TestClass1=2"} =
	getLdn(?CLIENT_0, ?ONESHOT, ObjectId2),
    {ok, ?CMSI_LDN_NOT_FOUND} = getLdn(?CLIENT_0, ?ONESHOT, ?BAD_OBJ_ID),

    {ok, ?CMSI_OK, ObjectId} =
	getObjectId(?CLIENT_0, ?ONESHOT,
		    "ManagedElement=1,TestRoot=1,TestClass1=1"),

    {ok, ?CMSI_OK, [ObjectId, ObjectId2]} =
	getObjectIds(?CLIENT_0, ?ONESHOT,
		     ["ManagedElement=1,TestRoot=1,TestClass1=1",
		      "ManagedElement=1,TestRoot=1,TestClass1=2"]),

    {ok, ?CMSI_OK, []} = getObjectIds(?CLIENT_0, ?ONESHOT, []),

    {ok, ?CMSI_OK, ["ManagedElement=1,TestRoot=1,TestClass1=1",
    		    "ManagedElement=1,TestRoot=1,TestClass1=2"]} =
    	getLdns(?CLIENT_0, ?ONESHOT, [ObjectId, ObjectId2]),

    {ok, ?CMSI_OK, ["ManagedElement=1,TestRoot=1,TestClass1=1",
    		    ""]} =
    	getLdns(?CLIENT_0, ?ONESHOT, [ObjectId, ?BAD_OBJ_ID]),

    {ok, ?CMSI_OK, []} = getLdns(?CLIENT_0, ?ONESHOT, []),

    {ok} = initiateService(?CLIENT_1),

    {ok, ?CMSI_OK, ObjectId} =
	getObjectId(?CLIENT_1, ?LONGLIVING,
		    "ManagedElement=1,TestRoot=1,TestClass1=1"),
    {ok, ?CMSI_OK, ObjectId} =
	getObjectId(?CLIENT_1, ?LONGLIVING,
		    "ManagedElement=1,TestRoot=1,TestClass1=1"),
    {ok, ?CMSI_OK, ObjectId2} =
	getObjectId(?CLIENT_1, ?LONGLIVING,
		    "ManagedElement=1,TestRoot=1,TestClass1=2"),
    {ok, ?CMSI_OK, "ManagedElement=1,TestRoot=1,TestClass1=1"} =
	getLdn(?CLIENT_1, ?LONGLIVING, ObjectId),
    {ok, ?CMSI_OK, "ManagedElement=1,TestRoot=1,TestClass1=2"} =
	getLdn(?CLIENT_1, ?LONGLIVING, ObjectId2),
    {ok, ?CMSI_LDN_NOT_FOUND} = getLdn(?CLIENT_0, ?LONGLIVING, ?BAD_OBJ_ID),

    {ok, ?CMSI_OK, ObjectId} =
	getObjectId(?CLIENT_1, ?LONGLIVING,
		    "ManagedElement=1,TestRoot=1,TestClass1=1"),

    {ok, ?CMSI_OK, [ObjectId, ObjectId2]} =
	getObjectIds(?CLIENT_1, ?LONGLIVING,
		     ["ManagedElement=1,TestRoot=1,TestClass1=1",
		      "ManagedElement=1,TestRoot=1,TestClass1=2"]),

    {ok, ?CMSI_OK, []} = getObjectIds(?CLIENT_1, ?LONGLIVING, []),

    {ok, ?CMSI_OK, ["ManagedElement=1,TestRoot=1,TestClass1=1",
		    "ManagedElement=1,TestRoot=1,TestClass1=2"]} =
	getLdns(?CLIENT_1, ?LONGLIVING, [ObjectId, ObjectId2]),

    {ok, ?CMSI_OK, ["ManagedElement=1,TestRoot=1,TestClass1=1",
    		    ""]} =
    	getLdns(?CLIENT_1, ?LONGLIVING, [ObjectId, ?BAD_OBJ_ID]),

    {ok, ?CMSI_OK, []} = getLdns(?CLIENT_1, ?LONGLIVING, []),

    {ok} = terminateService(?CLIENT_1),

    ok.

-spec cmsi_get_struct_ref_for_attr(config()) -> ok.
cmsi_get_struct_ref_for_attr(_Config) ->
    {ok, ?CMSI_OK, "TESTMOMStruct3"} = getStructRefForAttr(?CLIENT_0, ?ONESHOT,
    							   "TESTMOMTestClass3",
    							   "struct3seq"),

    {ok, ?CMSI_CLASS_NOT_FOUND, undefined} =
    	getStructRefForAttr(?CLIENT_0, ?ONESHOT, "notAClass", "notAnAttr"),
    {ok, ?CMSI_ATTRIBUTE_NOT_FOUND, undefined} =
    	getStructRefForAttr(?CLIENT_0, ?ONESHOT, "TESTMOMTestClass3",
    			    "notAnAttr"),
    {ok, ?CMSI_ATTRIBUTE_NO_STRUCT_REF, undefined} =
    	getStructRefForAttr(?CLIENT_0, ?ONESHOT, "TESTMOMTestClass3",
    			    "managedElement"),
    {ok, ?CMSI_ATTRIBUTE_NO_STRUCT_REF, undefined} =
    	getStructRefForAttr(?CLIENT_0, ?ONESHOT, "TESTMOMStruct1",
    			    "struct1mem1"),

    {ok} = initiateService(?CLIENT_1),

    {ok, ?CMSI_OK, "TESTMOMStruct3"} = getStructRefForAttr(?CLIENT_1,
    							   ?LONGLIVING,
    							   "TESTMOMTestClass3",
    							   "struct3seq"),

    {ok, ?CMSI_CLASS_NOT_FOUND, undefined} =
    	getStructRefForAttr(?CLIENT_1, ?LONGLIVING, "notAClass", "notAnAttr"),
    {ok, ?CMSI_ATTRIBUTE_NOT_FOUND, undefined} =
    	getStructRefForAttr(?CLIENT_1, ?LONGLIVING, "TESTMOMTestClass3",
    			    "notAnAttr"),
    {ok, ?CMSI_ATTRIBUTE_NO_STRUCT_REF, undefined} =
    	getStructRefForAttr(?CLIENT_1, ?LONGLIVING, "TESTMOMTestClass3",
    			    "managedElement"),
    {ok, ?CMSI_ATTRIBUTE_NO_STRUCT_REF, undefined} =
    	getStructRefForAttr(?CLIENT_1, ?LONGLIVING, "TESTMOMStruct1",
    			    "struct1mem1"),

    {ok} = terminateService(?CLIENT_1),

    ok.

-spec cmsi_is_attr_ref2struct(config()) -> ok.
cmsi_is_attr_ref2struct(_Config) ->
    {ok, ?CMSI_OK, false} = isAttrRef2Struct(?CLIENT_0, ?ONESHOT,
					     "TESTMOMTestClass3",
					     "managedElement"),
    {ok, ?CMSI_OK, true} = isAttrRef2Struct(?CLIENT_0, ?ONESHOT,
					    "TESTMOMTestClass3", "struct3seq"),
    {ok, ?CMSI_OK, false} = isAttrRef2Struct(?CLIENT_0, ?ONESHOT,
					     "TESTMOMStruct1", "struct1mem1"),

    {ok, ?CMSI_CLASS_NOT_FOUND, _} =
	isAttrRef2Struct(?CLIENT_0, ?ONESHOT, "notAClass", "notAnAttr"),
    {ok, ?CMSI_ATTRIBUTE_NOT_FOUND, _} =
	isAttrRef2Struct(?CLIENT_0, ?ONESHOT, "TESTMOMTestClass3", "notAnAttr"),

    {ok} = initiateService(?CLIENT_1),

    {ok, ?CMSI_OK, false} = isAttrRef2Struct(?CLIENT_1, ?LONGLIVING,
					     "TESTMOMTestClass3",
					     "managedElement"),
    {ok, ?CMSI_OK, true} = isAttrRef2Struct(?CLIENT_1, ?LONGLIVING,
					    "TESTMOMTestClass3", "struct3seq"),
    {ok, ?CMSI_OK, false} = isAttrRef2Struct(?CLIENT_1, ?LONGLIVING,
					     "TESTMOMStruct1", "struct1mem1"),

    {ok, ?CMSI_CLASS_NOT_FOUND, _} =
	isAttrRef2Struct(?CLIENT_1, ?LONGLIVING, "notAClass", "notAnAttr"),
    {ok, ?CMSI_ATTRIBUTE_NOT_FOUND, _} =
	isAttrRef2Struct(?CLIENT_1, ?LONGLIVING, "TESTMOMTestClass3",
			 "notAnAttr"),

    {ok} = terminateService(?CLIENT_1),

    ok.

-spec cmsi_get_dn_prefix(config()) -> ok.
cmsi_get_dn_prefix(_Config) ->
    NewDnPrefix = "DC=ericsson.se,g3SubNetwork=Sweden",
    NewDnPrefixLen = length(NewDnPrefix),

    LongSize = 100,
    ShortSize = 10,
    TruncDnPrefix = lists:sublist(NewDnPrefix, ShortSize-1),

    {ok} = initiateService(?CLIENT_1),

    OrigDnPrefix = get_dnPrefix(),

    ok = set_dnPrefix(NewDnPrefix),

    {ok, ?CMSI_OK, NewDnPrefix, NewDnPrefixLen} =
	getDnPr(?CLIENT_0, ?ONESHOT, LongSize),
    {ok, ?CMSI_OK, NewDnPrefix, NewDnPrefixLen} =
	getDnPr(?CLIENT_1, ?LONGLIVING, LongSize),

    %% Check truncation
    {ok, ?CMSI_OK, TruncDnPrefix, NewDnPrefixLen} =
	getDnPr(?CLIENT_0, ?ONESHOT, ShortSize),
    {ok, ?CMSI_OK, TruncDnPrefix, NewDnPrefixLen} =
	getDnPr(?CLIENT_1, ?LONGLIVING, ShortSize),

    ok = set_dnPrefix(OrigDnPrefix),

    {ok} = terminateService(?CLIENT_1),

    ok.

%%% ----------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


%% @doc Set up a long-lived connection.

-spec initiateService(integer()) -> tuple().

initiateService(ClientId) ->
    send(ClientId,
	 ?CMSI_INITIATE_SERVICE,
	 ?IGNORED_TRANSPORT,
	 ?IGNORED_DIRECTION,
	 ?IGNORED_DN).


%% @doc Tear down a long-lived connection.

-spec terminateService(integer()) -> tuple().

terminateService(ClientId) ->
    send(ClientId,
	 ?CMSI_TERMINATE_SERVICE,
	 ?IGNORED_TRANSPORT,
	 ?IGNORED_DIRECTION,
	 ?IGNORED_DN).


%% @doc Transform a DN from 3GPP to IMM format.

-spec toImm(integer(), boolean(), string()) -> tuple().

toImm(ClientId, LongLiving, InDn) ->
    send(ClientId,
	 ?CMSI_TRANSFORM,
	 LongLiving,
	 ?MIM_TO_IMM,
	 InDn).

%% @doc Transform a DN from IMM to 3GPP format.

-spec to3gpp(integer(), boolean(), string()) -> tuple().

to3gpp(ClientId, LongLiving, InDn) ->
    send(ClientId,
	 ?CMSI_TRANSFORM,
	 LongLiving,
	 ?IMM_TO_MIM,
	 InDn).

%% @doc Check if an IMM class is a struct
-spec isMoClassStruct(integer(), boolean(), string()) -> tuple().
isMoClassStruct(ClientId, LongLiving, ClassName) ->
    rct_proxy:send_proxy(node1, cmsi1, ?CMSI_IS_MO_CLASS_STRUCT,
			 {ClientId, LongLiving, ClassName}).

%% @doc Check if an IMM class is a struct (Repeated)
-spec repeatIsMoClassStruct(integer(), boolean(), string(), tuple(), integer()) -> ok.
repeatIsMoClassStruct(_ClientId, _LongLiving, _ClassName, _Expect, 0) ->
    ok;
repeatIsMoClassStruct(ClientId, LongLiving, ClassName, Expect, N) ->
    Expect = isMoClassStruct(ClientId, LongLiving, ClassName),
    repeatIsMoClassStruct(ClientId, LongLiving, ClassName, Expect, N-1).

%% @doc Check if an IMM attribute is referring to a struct
-spec isAttrRef2Struct(integer(), boolean(), string(), string()) -> tuple().
isAttrRef2Struct(ClientId, LongLiving, ClassName, AttrName) ->
    rct_proxy:send_proxy(node1, cmsi1, ?CMSI_IS_ATTR_REF_2_STRUCT,
			 {ClientId, LongLiving, ClassName, AttrName}).

%% @doc Check if an IMM class is a struct (Repeated)
-spec repeatIsAttrRef2Struct(integer(), boolean(), string(), string(), tuple(), integer()) -> ok.
repeatIsAttrRef2Struct(_ClientId, _LongLiving, _ClassName, _AttrName, _Expect,
		       0) ->
    ok;
repeatIsAttrRef2Struct(ClientId, LongLiving, ClassName, AttrName, Expect, N) ->
    Expect = isAttrRef2Struct(ClientId, LongLiving, ClassName, AttrName),
    repeatIsAttrRef2Struct(ClientId, LongLiving, ClassName, AttrName, Expect,
			   N-1).

-spec getStructRefForAttr(integer(), boolean(), string(), string()) -> tuple().
getStructRefForAttr(ClientId, LongLiving, ClassName, AttrName) ->
    rct_proxy:send_proxy(node1, cmsi1, ?CMSI_GET_STRUCTREF_FOR_ATTR,
			 {ClientId, LongLiving, ClassName, AttrName}).

-spec repeatGetStructRefForAttr(integer(), boolean(), string(), string(), tuple(), integer()) -> ok.
repeatGetStructRefForAttr(_ClientId, _LongLiving, _ClassName, _AttrName,
			  _Expect, 0) ->
    ok;
repeatGetStructRefForAttr(ClientId, LongLiving, ClassName, AttrName, Expect, N) ->
    Expect = getStructRefForAttr(ClientId, LongLiving, ClassName, AttrName),
    repeatGetStructRefForAttr(ClientId, LongLiving, ClassName, AttrName,
			      Expect, N-1).

getObjectId(ClientId, LongLiving, Ldn) ->
    rct_proxy:send_proxy(node1, cmsi1, ?CMSI_GET_OBJECT_ID,
			 {ClientId, LongLiving, Ldn}).

getLdn(ClientId, LongLiving, ObjectId) ->
    rct_proxy:send_proxy(node1, cmsi1, ?CMSI_GET_LDN,
			 {ClientId, LongLiving, ObjectId}).

getObjectIds(ClientId, LongLiving, Ldns) ->
    rct_proxy:send_proxy(node1, cmsi1, ?CMSI_GET_OBJECT_IDS,
			 {ClientId, LongLiving, Ldns}).

getLdns(ClientId, LongLiving, ObjectIds) ->
    rct_proxy:send_proxy(node1, cmsi1, ?CMSI_GET_LDNS,
			 {ClientId, LongLiving, ObjectIds}).

getNmei(ClientId, LongLiving, Size) ->
    rct_proxy:send_proxy(node1, cmsi1, ?CMSI_GET_ME_ID,
			 {ClientId, LongLiving, Size}).

getAdmOpId(ClientId, LongLiving, ClassName, ActionName) ->
    rct_proxy:send_proxy(node1, cmsi1, ?CMSI_GET_ADM_OP_ID,
			 {ClientId, LongLiving, {ClassName, ActionName}}).

getActionName(ClientId, LongLiving, ClassName, AdmOpId) ->
    rct_proxy:send_proxy(node1, cmsi1, ?CMSI_GET_ACTION_NAME,
			 {ClientId, LongLiving, {ClassName, AdmOpId}}).

getOapData(ClientId, LongLiving) ->
    rct_proxy:send_proxy(node1, cmsi1, ?CMSI_GET_OAP_DATA, {ClientId, LongLiving}).

getMeUl(ClientId, LongLiving, Size) ->
    rct_proxy:send_proxy(node1, cmsi1, ?CMSI_GET_ME_USER_LABEL,
			 {ClientId, LongLiving, Size}).

getImmClassName(ClientId, LongLiving, Dn) ->
    rct_proxy:send_proxy(node1, cmsi1, ?CMSI_GET_IMM_CLASS_NAME,
			 {ClientId, LongLiving, Dn}).

getOapData2(ClientId, LongLiving) ->
    rct_proxy:send_proxy(node1, cmsi1, ?CMSI_GET_OAP_DATA2, {ClientId, LongLiving}).

getBidirAss(ClientId, LongLiving, ImmClass) ->
    rct_proxy:send_proxy(node1, cmsi1, ?CMSI_GET_BIDIR_ASS_FOR_CLIENT,
			 {ClientId, LongLiving, ImmClass}).

getDnPr(ClientId, LongLiving, Size) ->
    rct_proxy:send_proxy(node1, cmsi1, ?CMSI_GET_DN_PREFIX,
			 {ClientId, LongLiving, Size}).

%% @doc Send a request N times. A long-living connection is assumed.

-spec repeatSend(integer(), integer(), integer(), string(), string()) -> ok.

repeatSend(0, _ClientId, _Direction, _InDn, _OutDn) ->
    ok;

repeatSend(N, ClientId, Direction, InDn, OutDn) ->
    {ok, ?CMSI_OK, OutDn} =
	send(ClientId, ?CMSI_TRANSFORM, ?LONGLIVING, Direction, InDn),
    repeatSend(N-1, ClientId, Direction, InDn, OutDn).


%% @doc Send a request to the IFT application.

-spec send(integer(), integer(), boolean(), integer(), string()) -> tuple().

send(ClientId, Function, LongLivingTn, Direction, InDn) ->
    rct_proxy:send_proxy(node1,
			 cmsi1,
			 Function,
			 {ClientId, LongLivingTn, Direction, length(InDn), InDn}).


%% @doc Starts an HTTP service for the interactive test case.

-spec startHttpd() -> pid().

startHttpd() ->
    ct:print("startHttpd()", []),
    ServerDir = "/dev/shm/"++os:getenv("LOGNAME")++"/server",
    ct:print("startHttpd() 1", []),
    os:cmd(io_lib:format("mkdir -p ~s", [ServerDir])),
    ct:print("startHttpd() 2", []),
    {ok, Pid} = inets:start(httpd,
			    [{modules, [mod_esi]},
			     {port, ?CMSI_INTERACTIVE_HTTP_PORT},
			     {server_name, "cmsi_interactive"},
			     {server_root, ServerDir},
			     {document_root, "/"},
			     {bind_address, any},
			     {ipfamily, inet},
			     {erl_script_alias, {"/esi", [cmsi_c_SUITE, io]}},
			     {erl_script_nocache, true}
			    ]),
    ct:print("startHttpd() 3", []),
    ct:print("services info: ~p", [inets:services_info()]),
    ct:print("to continue this testcase point a browser to http://localhost:~w/esi/~s/handleForm",
	     [?CMSI_INTERACTIVE_HTTP_PORT, ?MODULE]),
    Pid.


%% @doc Handles a GET request and delivers an HTML page in response.

-spec handleForm(term(), list(), string()) -> ok | {error, term()}.

handleForm(SessionID, Env, RawInput) ->
    {_, HttpHost} = lists:keyfind(http_host, 1, Env),
    Url = io_lib:format("http://~s/esi/~s/handleForm", [HttpHost, ?MODULE]),
    Input = httpd:parse_query(RawInput),

    Substs = [
	      {url, Url},
	      {to_imm, integer_to_list(?MIM_TO_IMM)},
	      {to_3gpp, integer_to_list(?IMM_TO_MIM)},
	      {module, atom_to_list(?MODULE)},
	      {now, io_lib:format("~p", [os:timestamp()])},
	      {input, io_lib:format("~p", [Input])}
	     ],

    case lists:keyfind("quit", 1, Input) of
	{_, "true"} ->
	    mod_esi:deliver(SessionID,
			    ["Content-Type: text/html\r\n\r\n",
			     byePage()
			    ]),
	    cmsi_interactive ! finish;
	_ ->
	    case lists:keyfind("translate", 1, Input) of
		{_, "true"} ->

		    {_, Direction} = lists:keyfind("direction", 1, Input),
		    {_, InputDn} = lists:keyfind("inputDn", 1, Input),

		    {ok, Status, OutputDn} =
			send(?CLIENT_0, ?CMSI_TRANSFORM, false, list_to_integer(Direction), InputDn),

		    OutputDnForDisplay = if Status =:= ?CMSI_OK -> OutputDn; true -> "" end,

		    MoreSubsts = [{status, decodeStatus(Status)}, {outputDn, OutputDnForDisplay}|Substs],

		    mod_esi:deliver(SessionID, [
						"Content-Type: text/html\r\n\r\n",
						subst(MoreSubsts, translationForm(), [])
					       ]);
		_ -> % not supposed to happen!?
		    mod_esi:deliver(SessionID, [
						"Content-Type: text/html\r\n\r\n",
						subst(Substs, translationForm(), [])
					       ])

	    end
    end.



%% @doc Returns a list of strings and atoms that is an HTML page.

-spec byePage() -> [atom() | string()].

byePage() ->
    [
     "<html><head><title>Thank you</title></head>",
     "<body><big>Thank you for using RBS CS.</big></body></html>"
    ].


%% @doc Returns a list of strings and atoms that is an HTML page
%% template. Atoms are intended to be replaced by strings before the
%% document is delivered.

-spec translationForm() -> [atom() | string()].

translationForm() ->
    [
     "<!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN'>",
     "<html>",
     "  <head>",
     "    <meta content='text/html; charset=ISO-8859-1'",
     "      http-equiv='Content-Type'>",
     "    <title>Distinguished Name Translation</title>",
     "  </head>",
     "  <body>",
     "    <h2> Distinguished Name Translation</h2>",
     "    <p><big>using the CMSI interface on RBS CS</big><br>",
     "    </p>",
     "    <table border='3' cellpadding='2' cellspacing='2' width='100%'>",
     "      <tbody>",
     "        <tr>",
     "          <td valign='top' bgcolor='#bbffff'> <big>",
     "              <form action='", url,"'",
     "                method='get'><br>",
     "                <input name='direction' value='",to_imm,"' checked='checked'",
     "                  type='radio'> 3GPP -&gt; IMM<br>",
     "                <input name='direction' value='",to_3gpp,"' type='radio'> IMM",
     "                -&gt; 3GPP<br>",
     "                <br>",
     "                &nbsp;<input name='inputDn' size='80' maxlength='80'",
     "                  type='text'><br>",
     "                <br>",
     "                <button name='translate' value='true' type='submit'>Translate</button>",
     "                <button name='quit' value='true' type='submit'>Quit</button><br>",
     "                <br>",
     "                Status: ",status,"<br>",
     "                <br>",
     "                Translation: ",outputDn,"<br>",
     "                <br>",
     "              </form>",
     "            </big> </td>",
     "        </tr>",
     "      </tbody>",
     "    </table>",
     now, "<br>",
     input, "<br>",
     "  </body>",
     "</html>"
    ].


%% @doc Replaces atoms by strings in the given list of atoms and strings.
%% The first argument is the translations that can be made.
%% The second argument is the "input document" to be translated. The
%% third argument is a prefix document in reverse order that will not
%% be translated (typically the empty list).
%%
%% No check is made that the given translations are used. An atom that
%% cannot be looked up in the translation table causes an empty string
%% to be inserted.

-spec subst([{atom(), string()}], [atom() | string()], [string()]) -> [string()].

subst(_Translations, [], Result) ->
    lists:reverse(Result);

subst(Translations, [Item|Tail], Result) when is_atom(Item) ->
    case lists:keyfind(Item, 1, Translations) of
	false ->
	    subst(Translations, Tail, [""|Result]);
	{_, Replacement} ->
	    subst(Translations, Tail, [Replacement|Result])
    end;

subst(Translations, [Item|Tail], Result) ->
    subst(Translations, Tail, [Item|Result]).


%% @doc Translates status code to symbolic name. This function
%% must match the set of 'define' directives for the status codes.

-spec decodeStatus(integer()) -> string().

decodeStatus(?CMSI_OK)                          ->
    "CMSI_OK";
decodeStatus(?CMSI_INVALID_PARAMETER_DIRECTION) ->
    "CMSI_INVALID_PARAMETER_DIRECTION";
decodeStatus(?CMSI_INVALID_PARAMETER_TODNP)     ->
    "CMSI_INVALID_PARAMETER_TODNP";
decodeStatus(?CMSI_SEND_ERROR)                  ->
    "CMSI_SEND_ERROR";
decodeStatus(?CMSI_RECEIVE_ERROR)               ->
    "CMSI_RECEIVE_ERROR";
decodeStatus(?CMSI_OBJECT_CLASS_NOT_FOUND)      ->
    "CMSI_OBJECT_CLASS_NOT_FOUND";
decodeStatus(_Status)                           ->
    "undefined".

set_networkManagedElementId(MeId, NmeId) ->
    Edit = {'ManagedElement',
            [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
            [{managedElementId,[],[MeId]},
             {networkManagedElementId, [NmeId]}
            ]},

    {ok, _} = open_trans(),
    ok = ct_netconfc:edit_config(nc1, running, Edit),
    ok = close_trans(),
    ok = timer:sleep(?SLEEP_AFTER_EDIT).

set_managedElementUserLabel(UserLabel) ->
    Edit =
	if
	    is_list(UserLabel) ->
		{'ManagedElement',
		 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		 [{managedElementId,[],["1"]},
		  {userLabel, [UserLabel]}
		 ]};
	    UserLabel == undefined ->
		{'ManagedElement',
		 [{xmlns,"urn:com:ericsson:ecim:ComTop"},
                  {'xmlns:xc',"urn:ietf:params:xml:ns:netconf:base:1.0"}],
		 [{managedElementId,[],["1"]},
		  {userLabel,[{'xc:operation',"delete"}],[]}
		 ]}
	end,

    {ok, _} = open_trans(),
    ok = ct_netconfc:edit_config(nc1, running, Edit),
    ok = close_trans(),
    ok = timer:sleep(?SLEEP_AFTER_EDIT).

get_managedElementUserLabel() ->
    {ok, _} =  open_trans(),

    Get = {'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],["1"]},
	    {userLabel, []}]
	  },
    UserLabel =
	case ct_netconfc:get(nc1, Get) of
	    {ok, [{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		   [{managedElementId,[],["1"]},
		    {userLabel,[],[U]}]}]}  ->
		U;
	    {ok, [{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		   [{managedElementId,[],["1"]},
		    {userLabel,[],[]}]}]}  ->
		"";
	    {ok, [{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		   [{managedElementId,[],["1"]},
		    {userLabel,[{unset,"true"}],[]}]}]} ->
		undefined
	end,
    ok = close_trans(),
    UserLabel.

get_dnPrefix() ->
    {ok, _} =  open_trans(),

    Get = {'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],["1"]},
	    {dnPrefix, []}]
	  },
    DnPrefix =
	case ct_netconfc:get(nc1, Get) of
	    {ok, [{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		   [{managedElementId,[],["1"]},
		    {dnPrefix,[],[U]}]}]}  ->
		U;
	    {ok, [{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		   [{managedElementId,[],["1"]},
		    {dnPrefix,[],[]}]}]}  ->
		"";
	    {ok, [{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		   [{managedElementId,[],["1"]},
		    {dnPrefix,[{unset,"true"}],[]}]}]} ->
		undefined
	end,
    ok = close_trans(),
    DnPrefix.

set_dnPrefix(DnPrefix) ->
    Edit =
	if
	    is_list(DnPrefix) ->
		{'ManagedElement',
		 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		 [{managedElementId,[],["1"]},
		  {dnPrefix, [DnPrefix]}
		 ]};
	    DnPrefix == undefined ->
		{'ManagedElement',
		 [{xmlns,"urn:com:ericsson:ecim:ComTop"},
                  {'xmlns:xc',"urn:ietf:params:xml:ns:netconf:base:1.0"}],
		 [{managedElementId,[],["1"]},
		  {dnPrefix,[{'xc:operation',"delete"}],[]}
		 ]}
	end,

    {ok, _} = open_trans(),
    ok = ct_netconfc:edit_config(nc1, running, Edit),
    ok = close_trans(),
    ok = timer:sleep(?SLEEP_AFTER_EDIT).

create_ip_instances() ->
    IsTarget = is_target(),
    create_ip_instances(IsTarget).

create_ip_instances(true) ->
    IPv4 = get_new_ipv4_addr(),
    LdnV4 =
	"ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=1,AddressIPv4=1",
    open_trans(),
    create_ipv4_instance(IPv4),
    ok = close_trans(),
    ok = timer:sleep(?SLEEP_AFTER_EDIT),
    [{'AF_INET', IPv4, LdnV4}];
create_ip_instances(false) ->
    IPv4 = get_new_ipv4_addr(),
    LdnV4 =
	"ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=1,AddressIPv4=1",
    IPv6 = get_new_ipv6_addr(),
    LdnV6 =
	"ManagedElement=1,Transport=1,Router=1,InterfaceIPv6=1,AddressIPv6=1",

    open_trans(),
    create_ipv4_instance(IPv4),
    create_ipv6_instance(IPv6),
    ok = close_trans(),
    ok = timer:sleep(?SLEEP_AFTER_EDIT),
    [{'AF_INET6', IPv6, LdnV6}, {'AF_INET', IPv4, LdnV4}].

create_ipv4_instance(Address) ->
    ok = ct_netconfc:edit_config(?NETCNF, running,
                                 {'ManagedElement', [?COMTOP],
                                  [{managedElementId, [], ["1"]},
                                   {'Transport', [],
                                    [{transportId, [], ["1"]},
                                     {'Router', [],
                                      [{routerId, [], ["1"]},
                                       {'InterfaceIPv4', [],
                                        [{interfaceIPv4Id, [], ["1"]},
                                         {'AddressIPv4', [],
                                          [{addressIPv4Id, [], ["1"]},
                                           {address, [], [Address ++ "/32"]}]}]
                                       }]
                                     }]}]
                                 }).

create_ipv6_instance(Address) ->
    ok = ct_netconfc:edit_config(?NETCNF, running,
                                 {'ManagedElement', [?COMTOP],
                                  [{managedElementId, [], ["1"]},
                                   {'Transport', [],
                                    [{transportId, [], ["1"]},
                                     {'Router', [],
                                      [{routerId, [], ["1"]},
                                       {'InterfaceIPv6', [],
                                        [{interfaceIPv6Id, [], ["1"]},
                                         {'AddressIPv6', [],
                                          [{addressIPv6Id, [], ["1"]},
                                           {address, [], [Address ++ "/128"]}]}]
                                       }]
                                     }]}]
                                 }).

get_OamAccessPoint(Ldn) ->
    {'OamAccessPoint', [],
     [{oamAccessPointId, [], ["1"]}, {accessPoint, [], [Ldn]}]
    }.

get_OamAccessPoint() ->
    open_trans(),
    {ok, [{'ManagedElement', _, [{managedElementId,[],["1"]},
				 {'SystemFunctions',[],
				  [{systemFunctionsId,[],["1"]},
				   {'SysM', _,
				    [{sysMId,[],["1"]},
				     {'OamAccessPoint', _, OamAccessPointAttr} = OldOap]}
				  ]
				 }]
	  }]} =
	ct_netconfc:get_config(?NETCNF, running,
                               {'ManagedElement', [?COMTOP],
                                [{managedElementId, [], ["1"]},
                                 {'SystemFunctions', [],
                                  [{systemFunctionsId, [], ["1"]},
                                   {'SysM', [],
				    [{sysMId, [], ["1"]},
				     {'OamAccessPoint', [],
				      [{oamAccessPointId, [],["1"]}]}]}
				  ]
                                 }]
                               }),
    ok = close_trans(),

    case lists:keysearch(accessPoint, 1, OamAccessPointAttr) of
	false ->
	    Delete = {accessPoint, ?DELETE, []},
	    OapId = lists:keyfind(oamAccessPointId, 1, OamAccessPointAttr),
	    {'OamAccessPoint', [], [OapId, Delete]};
	{value, _} ->
	    OldOap
    end.

get_dscp({'OamAccessPoint', _, AttrList}) ->
    case lists:keyfind(dscp, 1, AttrList) of
	{dscp, [], [DSCPstring]} -> list_to_integer(DSCPstring);
	false -> 0
    end.

set_OamAccessPoint(Oap, Timeout) ->
    open_trans(Timeout),
    ok = ct_netconfc:edit_config(?NETCNF, running,
                                 {'ManagedElement', [?COMTOP],
                                  [{managedElementId, [], ["1"]},
                                   {'SystemFunctions', [],
				    [{systemFunctionsId, [], ["1"]},
				     {'SysM', [], [{sysMId, [], ["1"]}, Oap]}
				    ]
                                   }
				  ]
                                 }),
    ok = close_trans(),
    ok = timer:sleep(?SLEEP_AFTER_EDIT).

get_new_ipv4_addr() ->
    case is_target() of
        true ->
            {ok, IP} = rct_cli_coli_common:get_ip(?CLI),
            IP;
        false ->
	    {ok, Host} = inet:gethostname(),
	    {ok, IP} = inet:getaddr(Host, inet),
	    inet:ntoa(IP)
    end.

get_new_ipv6_addr() ->
    case is_target() of
        false ->
	    {ok, Host} = inet:gethostname(),
	    {ok, IP} = inet:getaddr(Host, inet6),
	    inet:ntoa(IP)
    end.

is_target() -> "target" == os:getenv("SIM_OR_TARGET").

open_trans(Timeout) ->
    Start = erlang:monotonic_time(second),
    open_trans(Start, Timeout).

open_trans(Start, Timeout) ->
    Curr = erlang:monotonic_time(second),
    case open_trans() of
	ok ->
	    ok;
	R when Curr > Start+Timeout ->
	    R;
	_ ->
	    timer:sleep(1000),
	    open_trans(Start, Timeout)
    end.

open_trans() -> ct_netconfc:open(?NETCNF, []).

close_trans() -> ct_netconfc:close_session(?NETCNF).

get_ipv4_32bits('AF_INET', Address) ->
    {ok, {A,B,C,D}} = inet:parse_ipv4strict_address(Address),
    (A bsl 24) + (B bsl 16) + (C bsl 8) + D;
get_ipv4_32bits('AF_INET6', _) ->
    0.

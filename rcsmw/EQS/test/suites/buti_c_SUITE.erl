%%% ----------------------------------------------------------
%%% %CCaseFile:	buti_c_SUITE.erl %
%%% Author:	erarafo
%%% @author erarafo
%%% Description:
%%% @doc ==Basic Test Suite for the BUTI legacy C interface on SUT.==
%%% This Test Suite can be used on both target and simulated enviroment.
%%% <br/><br/>
%%% rct_proxy is used in ct_hooks:
%%% see [https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/edoc/rct_proxy.html]
%%% <br/>
%%% @end
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(buti_c_SUITE).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R8A/2').
-date('2017-01-18').
-author('erarafo').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/3 %
%%%
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
%%%
%%% ----------------------------------------------------------
%%% REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R3A/1      2015-07-13 etxjovp     Add group definitions used by CS CI
%%% R4A/1      2015-09-17 etxpejn     Added cluster sim groups
%%% R8A/1      2017-01-18 erarafo     Correct copyright, add export directives
%%% R8A/2      2017-01-18 erarafo     Optional enhanced logging


-include_lib("common_test/include/ct.hrl").
-include("test_buti.hrl").

% IFT_APP protocol for controlling ift_app itself, see master.h
-define(SELF, 16).

% Function for setting debug level, see test_self.c
-define(SELF_SET_APPLOG_LEVEL, 12).

% Debug levels settable in ift_app, see test_self.c
-define(A_ERROR, 1).
-define(A_WARNING, 2).
-define(A_INFO, 3).
-define(A_DEBUG, 4).


-export([incor_prot_vers/1,
	 cor_prot_vers/1,
	 subscribe_button_event/1,
	 unsubscribe_button_event/1,
	 change_feedback_mode/1,
	 feedback_blink_mode_on/1,
	 feedback_blink_mode_off/1,
	 check_subscriber/1,
	 check_no_subscriber/1]).

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 groups/0,
	 all/0]).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%  Returns list of tuples to set default properties
%%  for the suite.
%%
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%
%% @spec suite() -> Info
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {minutes, 30}},
     {ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc},
                 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
					       []
                                              }}]}},
                 {rct_proxy,[{1, node1, ssh_lmt_ipv4, du1, username}]},
                 {rct_core,[]}
                ]}].

%%--------------------------------------------------------------------
%% @doc
%% Initialization before the whole suite
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    
    % Enable these lines for enhanced logging to rcs/applicationlogs/*/ift_app.log
    %{ok, client_started} = rct_proxy:start_proxy(node1, buti0, ?SELF),
    %{ok} = rct_proxy:send_proxy(node1, buti0, ?SELF_SET_APPLOG_LEVEL, {?A_DEBUG}, 5000),
    %{ok, client_stopped} = rct_proxy:stop_proxy(node1, buti0),

    {ok, client_started} = rct_proxy:start_proxy(node1, buti1, ?BUTI),
    {ok, memory_initiated} =
        rct_proxy:send_proxy(node1, buti1, ?CelloButi_initiateMemory),
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after the whole suite
%%
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_suite(Config) -> ok
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    {ok, ?CELLO_BUTI_OK} =
        rct_proxy:send_proxy(node1, buti1, ?CelloButi_freeMemory),
    {ok, client_stopped} = rct_proxy:stop_proxy(node1, buti1),
    ok = rct_proxy:exit_master(node1).

%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case group.
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case group.
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case
%%
%% TestCase - atom()
%%   Name of the test case that is about to be run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case
%%
%% TestCase - atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of test case group definitions.
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% @spec: groups() -> [Group]
%% @end
%%--------------------------------------------------------------------
groups() ->
    AllGroup=all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__sim__1__group, [], [{group, default__group}]},
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__sim__1__group, [], [{group, default__group}]},
     {sdc__qual__sim__1__group, [], []},
     %% This suite can be run on both MPs within a sim cluster
     {sbc__cluster__dual_sim_1__1__group, [], [{group, default__group}]},
     {sbc__cluster__dual_sim_3__1__group, [], [{group, default__group}]}
    ].

%%--------------------------------------------------------------------
%% @doc
%%  Returns the list of groups and test cases that
%%  are to be executed.
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%% Reason = term()
%%   The reason for skipping all groups and test cases.
%%
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% @end
%%--------------------------------------------------------------------
all() ->
    [incor_prot_vers,
     cor_prot_vers,
     subscribe_button_event,
     unsubscribe_button_event,
     change_feedback_mode,
     feedback_blink_mode_on,
     feedback_blink_mode_off,
     check_subscriber,
     check_no_subscriber].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% The function is used to request connection to the BUTI Server, using incorrect protocols.<br/>
%% @spec incor_prot_vers(Config) -> ok
%% @end
%%--------------------------------------------------------------------
incor_prot_vers(_) ->
    ClientRef = 4711,
    HighestSupportedPV = ?CELLO_BUTI_PV1,
    RejectReason= ?CELLO_BUTI_INVALID_PV,
    {?CELLO_BUTI_INITIATE_SERVICE_REJ,
     _SignalRevision,
     HighestSupportedPV,
     RejectReason,
     ClientRef} =
	initiate_service({?CELLO_BUTI_NO_PV,
			  ?CELLO_BUTI_NO_PV,
			  ?CELLO_BUTI_NO_PV,
			  ClientRef}),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% The function is used to request connection to the BUTI Server, using correct protocols.<br/>
%% @spec cor_prot_vers(Config) -> ok
%% @end
%%--------------------------------------------------------------------
cor_prot_vers(_) ->
    ClientRef = 4711,
    SelectedPV = ?CELLO_BUTI_PV1,

    {?CELLO_BUTI_INITIATE_SERVICE_CFM,
     _SignalRevision,
     SelectedPV,
     ClientRef} =
	initiate_service({?CELLO_BUTI_PV1,
			  ?CELLO_BUTI_NO_PV,
			  ?CELLO_BUTI_NO_PV,
			  ClientRef}),

    {?CELLO_BUTI_TERMINATE_SERVICE_CFM,
     ClientRef} = terminate_service({ClientRef}),

    ok.

subscribe_button_event(_) ->
    ClientRef = 4711,
    SelectedPV = ?CELLO_BUTI_PV1,

    {?CELLO_BUTI_INITIATE_SERVICE_CFM,
     _SignalRevision,
     SelectedPV,
     ClientRef} =
	initiate_service({?CELLO_BUTI_PV1,
			  ?CELLO_BUTI_NO_PV,
			  ?CELLO_BUTI_NO_PV,
			  ClientRef}),

    {ok, ?CELLO_BUTI_OK} =
	rct_proxy:send_proxy(node1, buti1, ?CelloButi_subscribeButtonEvent,
			     {ClientRef}),
    {ok, {?CELLO_BUTI_SUBSCRIBE_BUTTON_EVENT_CFM, ClientRef}} =
	rct_proxy:receive_proxy(),

    {?CELLO_BUTI_TERMINATE_SERVICE_CFM,
     ClientRef} = terminate_service({ClientRef}),

    ok.

unsubscribe_button_event(_) ->
    ClientRef = 4711,
    SelectedPV = ?CELLO_BUTI_PV1,

    {?CELLO_BUTI_INITIATE_SERVICE_CFM,
     _SignalRevision,
     SelectedPV,
     ClientRef} =
	initiate_service({?CELLO_BUTI_PV1,
			  ?CELLO_BUTI_NO_PV,
			  ?CELLO_BUTI_NO_PV,
			  ClientRef}),

    {ok, ?CELLO_BUTI_OK} =
	rct_proxy:send_proxy(node1, buti1, ?CelloButi_subscribeButtonEvent,
			     {ClientRef}),
    {ok, {?CELLO_BUTI_SUBSCRIBE_BUTTON_EVENT_CFM, ClientRef}} =
	rct_proxy:receive_proxy(),

    {ok, ?CELLO_BUTI_OK} =
	rct_proxy:send_proxy(node1, buti1, ?CelloButi_unsubscribeButtonEvent,
			     {ClientRef}),
    {ok, {?CELLO_BUTI_UNSUBSCRIBE_BUTTON_EVENT_CFM, ClientRef}} =
	rct_proxy:receive_proxy(),

    {?CELLO_BUTI_TERMINATE_SERVICE_CFM,
     ClientRef} = terminate_service({ClientRef}),

    ok.

change_feedback_mode(_) ->
    ClientRef = 4711,
    SelectedPV = ?CELLO_BUTI_PV1,

    {?CELLO_BUTI_INITIATE_SERVICE_CFM,
     _SignalRevision,
     SelectedPV,
     ClientRef} =
	initiate_service({?CELLO_BUTI_PV1,
			  ?CELLO_BUTI_NO_PV,
			  ?CELLO_BUTI_NO_PV,
			  ClientRef}),

    {ok, ?CELLO_BUTI_OK} =
	rct_proxy:send_proxy(node1, buti1, ?CelloButi_changeFeedBackMode,
			     {?CELLO_BUTI_NO_FEEDBACK_BLINK_MODE, ClientRef}),
    {ok, {?CELLO_BUTI_CHANGE_FEEDBACK_MODE_CFM,
	  ClientRef,
	  ?CELLO_BUTI_NO_FEEDBACK_BLINK_MODE}} =
	rct_proxy:receive_proxy(),

    {ok, ?CELLO_BUTI_OK} =
	rct_proxy:send_proxy(node1, buti1, ?CelloButi_changeFeedBackMode,
			     {?CELLO_BUTI_FEEDBACK_BLINK_MODE, ClientRef}),
    {ok, {?CELLO_BUTI_CHANGE_FEEDBACK_MODE_CFM,
	  ClientRef,
	  ?CELLO_BUTI_FEEDBACK_BLINK_MODE}} =
	rct_proxy:receive_proxy(),

    {ok, ?CELLO_BUTI_OK} =
	rct_proxy:send_proxy(node1, buti1, ?CelloButi_changeFeedBackMode,
			     {42, ClientRef}),
    {ok, {?CELLO_BUTI_CHANGE_FEEDBACK_MODE_REJ,
	  ?CELLO_BUTI_INTERNAL_ERROR,
	  ClientRef,
	  42}} =
	rct_proxy:receive_proxy(),

    {?CELLO_BUTI_TERMINATE_SERVICE_CFM,
     ClientRef} = terminate_service({ClientRef}),

    ok.

feedback_blink_mode_on(_) ->
    ClientRef = 4711,
    SelectedPV = ?CELLO_BUTI_PV1,

    {?CELLO_BUTI_INITIATE_SERVICE_CFM,
     _SignalRevision,
     SelectedPV,
     ClientRef} =
	initiate_service({?CELLO_BUTI_PV1,
			  ?CELLO_BUTI_NO_PV,
			  ?CELLO_BUTI_NO_PV,
			  ClientRef}),

    {ok, ?CELLO_BUTI_OK} =
	rct_proxy:send_proxy(node1, buti1, ?CelloButi_changeFeedBackMode,
			     {?CELLO_BUTI_FEEDBACK_BLINK_MODE, ClientRef}),
    {ok, {?CELLO_BUTI_CHANGE_FEEDBACK_MODE_CFM,
	  ClientRef,
	  ?CELLO_BUTI_FEEDBACK_BLINK_MODE}} =
	rct_proxy:receive_proxy(),

    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, [released], 10000),
    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, [pressed], 10000),
    ok = check_maintenance_led_behavior(fast_blink, any),

    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, [released], 10000),
    ok = check_maintenance_led_behavior(off, any),

    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, [pressed], 10000),
    ok = check_maintenance_led_behavior(fast_blink, any),

    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, ['2seconds'], 10000),
    ok = check_maintenance_led_behavior(slow_blink, any),

    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, [released], 10000),
    ok = check_maintenance_led_behavior(off, any),

    {?CELLO_BUTI_TERMINATE_SERVICE_CFM,
     ClientRef} = terminate_service({ClientRef}),

    ok.

feedback_blink_mode_off(_) ->
    ClientRef = 4711,
    SelectedPV = ?CELLO_BUTI_PV1,

    {?CELLO_BUTI_INITIATE_SERVICE_CFM,
     _SignalRevision,
     SelectedPV,
     ClientRef} =
	initiate_service({?CELLO_BUTI_PV1,
			  ?CELLO_BUTI_NO_PV,
			  ?CELLO_BUTI_NO_PV,
			  ClientRef}),

    {ok, ?CELLO_BUTI_OK} =
	rct_proxy:send_proxy(node1, buti1, ?CelloButi_changeFeedBackMode,
			     {?CELLO_BUTI_NO_FEEDBACK_BLINK_MODE, ClientRef}),
    {ok, {?CELLO_BUTI_CHANGE_FEEDBACK_MODE_CFM,
	  ClientRef,
	  ?CELLO_BUTI_NO_FEEDBACK_BLINK_MODE}} =
	rct_proxy:receive_proxy(),

    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, [released], 10000),
    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, [pressed], 10000),
    ok = check_maintenance_led_behavior(off, all),

    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, [released], 10000),
    ok = check_maintenance_led_behavior(off, all),

    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, [pressed], 10000),
    ok = check_maintenance_led_behavior(off, all),

    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, ['2seconds'], 10000),
    ok = check_maintenance_led_behavior(off, all),

    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, [released], 10000),
    ok = check_maintenance_led_behavior(off, all),

    {?CELLO_BUTI_TERMINATE_SERVICE_CFM,
     ClientRef} = terminate_service({ClientRef}),

    ok.

check_subscriber(_) ->
    ClientRef = 4711,
    SelectedPV = ?CELLO_BUTI_PV1,

    {?CELLO_BUTI_INITIATE_SERVICE_CFM,
     _SignalRevision,
     SelectedPV,
     ClientRef} =
	initiate_service({?CELLO_BUTI_PV1,
			  ?CELLO_BUTI_NO_PV,
			  ?CELLO_BUTI_NO_PV,
			  ClientRef}),

    {ok, ?CELLO_BUTI_OK} =
	rct_proxy:send_proxy(node1, buti1, ?CelloButi_subscribeButtonEvent,
			     {ClientRef}),
    {ok, {?CELLO_BUTI_SUBSCRIBE_BUTTON_EVENT_CFM, ClientRef}} =
	rct_proxy:receive_proxy(),

    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, [released], 10000),
    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, [pressed], 10000),
    {ok, {?CELLO_BUTI_EVENT_IND,
          ?CELLO_BUTI_BUTTON_PRESSED}} = rct_proxy:receive_proxy(),

    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, [released], 10000),
    {ok, {?CELLO_BUTI_EVENT_IND,
          ?CELLO_BUTI_BUTTON_SHORT_RELEASE}} = rct_proxy:receive_proxy(),

    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, [pressed], 10000),
    {ok, {?CELLO_BUTI_EVENT_IND,
          ?CELLO_BUTI_BUTTON_PRESSED}} = rct_proxy:receive_proxy(),

    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, ['2seconds'], 10000),
    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, [released], 10000),
    {ok, {?CELLO_BUTI_EVENT_IND,
          ?CELLO_BUTI_BUTTON_MEDIUM_RELEASE}} = rct_proxy:receive_proxy(),

    {?CELLO_BUTI_TERMINATE_SERVICE_CFM,
     ClientRef} = terminate_service({ClientRef}),

    ok.

check_no_subscriber(_) ->
    ClientRef = 4711,
    SelectedPV = ?CELLO_BUTI_PV1,

    {?CELLO_BUTI_INITIATE_SERVICE_CFM,
     _SignalRevision,
     SelectedPV,
     ClientRef} =
	initiate_service({?CELLO_BUTI_PV1,
			  ?CELLO_BUTI_NO_PV,
			  ?CELLO_BUTI_NO_PV,
			  ClientRef}),

    %% Since there is no subscriber, no signal should be sent when the state in
    %% the buti server is updated. There is a one second timeout to verify
    %% that no signal is sent.
    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, [released], 10000),
    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, [pressed], 10000),
    {error, timeout} = rct_proxy:receive_proxy(1000),

    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, [released], 10000),
    {error, timeout} = rct_proxy:receive_proxy(1000),

    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, [pressed], 10000),
    {error, timeout} = rct_proxy:receive_proxy(1000),

    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, ['2seconds'], 10000),
    ok = rct_rpc:call(rpc, eqs_buti_service, set_button_event, [released], 10000),
    {error, timeout} = rct_proxy:receive_proxy(1000),

    {?CELLO_BUTI_TERMINATE_SERVICE_CFM,
     ClientRef} = terminate_service({ClientRef}),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% initiate_service
%%
%% @spec initiate_service(Args) -> integer()
%% @end
%%--------------------------------------------------------------------
initiate_service(Args) ->
    {ok, ?CELLO_BUTI_OK} =
        rct_proxy:send_proxy(node1, buti1, ?CelloButi_initiateService, Args),
    {ok, Answer} = rct_proxy:receive_proxy(),
    {ok, ?CELLO_BUTI_OK} =
        rct_proxy:send_proxy(node1, buti1, ?CelloButi_internal),

    Answer.

%%--------------------------------------------------------------------
%% @doc
%% terminate_service
%%
%% @spec terminate_service(Args) -> integer()
%% @end
%%--------------------------------------------------------------------
terminate_service(Args) ->
    {ok, ?CELLO_BUTI_OK} =
        rct_proxy:send_proxy(node1, buti1, ?CelloButi_terminateService, Args),
    {ok, Answer} = rct_proxy:receive_proxy(),

    {ok, ?CELLO_BUTI_OK} =
        rct_proxy:send_proxy(node1, buti1, ?CelloButi_internal),

    Answer.

check_maintenance_led_behavior(WantedLedBehavior, Type) ->
    check_maintenance_led_behavior(WantedLedBehavior, Type, 10).

check_maintenance_led_behavior(WantedLedBehavior, any, N) ->
    case rct_rpc:call(rpc, eqs_mmi, get_led_behavior, [maintenance], 10000) of
	WantedLedBehavior ->
	    ok;
	LedBehavior when N =:= 0 ->
	    {error, LedBehavior};
	_ ->
	    timer:sleep(100),
	    check_maintenance_led_behavior(WantedLedBehavior, any, N-1)
    end;

check_maintenance_led_behavior(WantedLedBehavior, all, N) ->
    case rct_rpc:call(rpc, eqs_mmi, get_led_behavior, [maintenance], 10000) of
	WantedLedBehavior when N =:= 0 ->
	    ok;
	WantedLedBehavior ->
	    timer:sleep(100),
	    check_maintenance_led_behavior(WantedLedBehavior, all, N-1);
	LedBehavior ->
	    {error, LedBehavior}
    end.

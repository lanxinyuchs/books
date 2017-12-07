%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmTestLib.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R5A/R6A/R8A/R11A/3

%%% @doc ==Scheduled backup library==
%%% This module contains library functions for doing swm tests
%%% @end

-module(swmTestLib).

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
%%% R5A/1      2016-03-29 ekurnik     Created
%%% R6A/1      2016-08-22 etxkols     Git migration requires that CC paths is not used 
%%% -----   ---------  -------- ------------------------
%%% R8A/1   2016-11-16 etxberb  Replaced netconf with rpc in get_swm_mo/0.
%%% R8A/3   2016-11-25 etxberb  Added functions with argument 'Node'.
%%% R8A/4   2016-11-25 etxjotj  Removed get_swm_mo as it is unused
%%% R8A/5   2017-01-01 etxberb  Correction of check_action_capable_progress/2.
%%% R8A/6   2017-01-10 etxberb  Added fake_vnfm/0 & fake_vnfm/1.
%%% R11A/2  2017-10-05 etxjotj  Additions for AVC driven testing
%%% ----------------------------------------------------------

-include_lib("common_test/include/ct.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-include("RcsSwM.hrl").

%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([get_action_capable/0,
         get_action_capable/1,
         get_action_capable_info/0,
         get_action_capable_info/1,
         check_action_capable/1,
         check_action_capable/2,
         check_action_capable_info/1,
         check_action_capable_info/2,
         check_action_capable_progress/1,
	 fake_vnfm/0,
	 fake_vnfm/1,
         flush_mnesia_event_messages/0,
         map_current_action/1,
         wait_for_action_capable/0,
         wait_for_action_capable/1
         ]).

-export([wait_for_action_capable_e/0, 
	 wait_for_action_capable_e/1,
	 wait_for_WAIT/1]).

-define(RPC(N, M, F, A), rct_rpc:call(N, M, F, A, 10000)).

%% get_swm_mo() ->
%%     get_swm_mo(rpc_1).

get_swm_mo(Node) ->
    %% rct_rpc:call(Node, swmI, moGet_SwM, [], 10000).
    case rct_rpc:call(Node, sysEnv, rcs_mode_2, [], 10000) of
	vrcs -> undefined;
	target -> do_get_swm_mo();
	simulated -> do_get_swm_mo()
    end.

do_get_swm_mo() ->
    SwM = 
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],["1"]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'SwM',
	     [{swMId,[],["1"]}]}]}]},
    {ok, Result} = netconf(get, [nc1, SwM]),
    {ok, {_, _, Contents}} = extract_element('SwM', Result),
    Contents.

get_action_capable() ->
    get_action_capable(rpc_1).

get_action_capable(Node) ->
    case get_swm_mo(Node) of
	undefined ->
	    undefined;
	Result ->
	    {ok, {actionCapable, _, [ActionCapable]}} = 
		extract_element(actionCapable, Result),
	    ActionCapable
    end.
    
get_action_capable_info() ->
    get_action_capable_info(rpc_1).

get_action_capable_info(Node) ->
    case get_swm_mo(Node) of
	undefined -> undefined;
	Result ->
	    {ok, {actionCapableInfo, _, ActionCapableInfo}} = 
		extract_element(actionCapableInfo, [Result]),
	    ActionCapableInfo
    end.
    
check_action_capable(Expected) ->
    check_action_capable(rpc_1, Expected).

check_action_capable(Node, Expected) ->
    case get_action_capable(Node) of
	undefined -> 
	    %% ct:pal("actionCapable is not present in vrcs",[]),
	    ok;
	ActionCapable ->
	    verify_action_capable(Expected, ActionCapable)
    end.
    
verify_action_capable(ActionCapable, ActionCapable) ->
     %% ct:pal("actionCapable [~p] matches the expected value [~p]", [ActionCapable, ActionCapable]),
     ok;

verify_action_capable(Expected, ActionCapable) ->
    ct:pal("actionCapable [~p] does not match the expected value [~p]", [ActionCapable, Expected]),
    nok.

check_action_capable_info(Expected) ->
    check_action_capable_info(rpc_1, Expected).

check_action_capable_info(Node, Expected) ->
    case get_action_capable_info(Node) of
	undefined ->
	    %% ct:pal("actionCapableInfo is not present in vrcs",[]),
	    ok;
	ActionCapableInfo ->
	    verify_action_capable_info(Expected, ActionCapableInfo)
    end.

%% undefined in mnesia database is translated into empty string
verify_action_capable_info(ActionCapableInfo, undefined) ->
    verify_action_capable_info(ActionCapableInfo, "");

verify_action_capable_info(ActionCapableInfo, ActionCapableInfo) ->
    %% ct:pal("actionCapableInfo [~p] matches the expected value [~p]", 
    %% 	   [ActionCapableInfo, ActionCapableInfo]),
    ok;

verify_action_capable_info(Expected, ActionCapableInfo) ->
    {ok, RE} = re:compile(Expected),
    case re:run(ActionCapableInfo, RE) of
    {match, _} ->
        %% ct:pal("actionCapableInfo [~p] matches the expected value [~p]", 
	%%        [ActionCapableInfo, Expected]),
        ok;
    _Other ->
        ct:pal("actionCapableInfo [~p] does not match the expected value [~p]", 
	       [ActionCapableInfo, Expected]),
        nok
    end.

wait_for_action_capable() ->
    wait_for_action_capable(60000).

wait_for_action_capable(Node) when is_atom(Node) ->
    wait_for_action_capable(Node, 60000);
wait_for_action_capable(Timeout) when is_integer(Timeout) ->
    wait_for_action_capable(rpc_1, Timeout).

wait_for_action_capable(Node, Timeout) when Timeout < 0 ->
    check_action_capable(Node, "CAPABLE");
wait_for_action_capable(Node, Timeout) ->
    case check_action_capable(Node, "CAPABLE") of
	ok ->
	    ok;
	nok ->
	    timer:sleep(5000),
	    wait_for_action_capable(Node, Timeout - 5000)
    end.


%% Node should have activated AVC subscription using swm_event_lib

wait_for_action_capable_e() ->
%    ct:print("wait_for_action_capable_e~n",[]),
    wait_for_action_capable_e(10000).

wait_for_action_capable_e(Node) when is_atom(Node) ->
    wait_for_action_capable_e(Node, 60000);
wait_for_action_capable_e(Timeout) when is_integer(Timeout) ->
    wait_for_action_capable_e(rpc_1, Timeout).

wait_for_action_capable_e(Node, Timeout) when Timeout > 0->
    MS = erlang:monotonic_time(milli_seconds),
    case check_action_capable(Node, "CAPABLE") of
	ok ->
	    ok;
	undefined ->
	    ok;
	_ ->
	    ct:print("Waiting for action capable change~n",[]),
	    receive
		{avc, _, "SwM", "1", _} =AVC ->
		    ct:print("~p~n",[AVC]),
		    MS2 = erlang:monotonic_time(milli_seconds),
		    Elapsed = MS2 - MS,
		    wait_for_action_capable_e(Node, Timeout-Elapsed)
	    after Timeout ->
		    ct:print("~p~n",[process_info(self(), messages)]),
		    check_action_capable("CAPABLE")
	    end
    end;
wait_for_action_capable_e(Node, _) ->
    check_action_capable(Node, "CAPABLE").

    


%% The test suite should be subscribed to mnesia events from test server
check_action_capable_progress([{ActionCapableExpected,
				ActionCapableInfoExpected} | Tail]
			      = ExpectedEvents) ->
    receive
        {mnesia_table_event, {write,
			      swM,
			      #swM{actionCapable = ActionCapable,
				   actionCapableInfo = ActionCapableInfo}, 
                              [#swM{actionCapable = ActionCapable,
				    actionCapableInfo = ActionCapableInfo} | _],
			      _ActivityId}} ->
            %% no change in actionCapable
            ct:log("Recvd mnesia event where actionCapable has not changed: {~p, ~p}",
		   [map_action_capable_enum(ActionCapable), ActionCapableInfo]),
            check_action_capable_progress(ExpectedEvents);
        {mnesia_table_event, {write,
			      swM,
			      #swM{actionCapable = ActionCapable,
				   actionCapableInfo = ActionCapableInfo}, 
                              [#swM{actionCapable = _ActionCapableOld,
				    actionCapableInfo = _ActionCapableInfoOld}
			       | _],
			      _ActivityId}} ->
            %% action capable changed, should match the expected values
	    case
		{verify_action_capable(ActionCapableExpected,
				       map_action_capable_enum(ActionCapable)), 
		 verify_action_capable_info(ActionCapableInfoExpected,
					    ActionCapableInfo)}
		of
		{ok, ok} ->
		    check_action_capable_progress(Tail);
		_ ->
		    ct:log("Recvd mnesia event with unexpected actionCapable: {~p, ~p}",
			   [map_action_capable_enum(ActionCapable),
			    ActionCapableInfo]),
		    check_action_capable_progress(ExpectedEvents)
	    end;
        {mnesia_table_event, MnesiaEvt} ->
            %% other table events
            ct:log("Recvd mnesia event other: ~p", [MnesiaEvt]),
            check_action_capable_progress(ExpectedEvents)
    after
	10000 ->
	    %% if no new messages arrive after 10 seconds, exit with nok
	    ct:log("Timeout, no matching mnesia events after 10 seconds"),
	    nok
    end;
check_action_capable_progress([]) ->
    ok.

wait_for_WAIT(Timeout) ->
    case rct_rpc:call(rpc_1, sysEnv, rcs_mode_2, [], 10000) of
	vrcs -> ok;
	target -> do_wait_for_WAIT(Timeout);
	simulated -> do_wait_for_WAIT(Timeout)
    end.

do_wait_for_WAIT(Timeout) ->
    receive
	{avc, _, "SwM", _, [{actionCapable,[["WAIT"]]}]} ->
	    ok
    after Timeout ->
	    ct:fail("ActionCapable did not reach WAIT state")
    end.






%%--------------------------------------------------------------------
%% fake VNFM for vrcs
fake_vnfm() ->
    fake_vnfm(rpc_1).

fake_vnfm(Node) ->
    case ?RPC(Node, sysEnv, rcs_mode_2, []) of
	vrcs ->
	    ?RPC(Node, os, putenv, ["FAKE_VNFM", ""]);
	_ ->
	    ?RPC(Node, os, unsetenv, ["FAKE_VNFM"])
    end.

%%--------------------------------------------------------------------
flush_mnesia_event_messages() ->
    receive
        {mnesia_table_event, _} ->
            flush_mnesia_event_messages()
    after 0 ->
        ok
    end.

    
map_current_action("createBackup") ->
    "BackupManager::createBackup";
map_current_action("deleteBackup") ->
    "BackupManager::deleteBackup";
map_current_action("importBackup") ->
    "BackupManager::importBackup";
map_current_action("restoreBackup") ->
    "Backup::restore";
map_current_action("exportBackup") ->
    "Backup::export";
map_current_action("ACTIVATE") ->
    "BrmFailsafeBackup::activate";
map_current_action("DEACTIVATE") ->
    "BrmFailsafeBackup::deactivate";
map_current_action("SCHEDULED_BACKUP") ->
    "creating scheduled backup";
map_current_action("SCHEDULED_EXPORT") ->
    "exporting scheduled backup";
map_current_action("createUpgradePackage") ->
    "SwM::createUpgradePackage";
map_current_action("prepare") ->
    "UpgradePackage::prepare";
map_current_action("verify") ->
    "UpgradePackage::verify";
map_current_action("activate") ->
    "UpgradePackage::activate";
map_current_action("confirm") ->
    "UpgradePackage::confirm";
map_current_action("internal_housekeeping") ->
    "internal housekeeping";
map_current_action(Other) ->
    ct:log("Mapping action ~p to actionCapableInfo not defined", [Other]),
    Other.

map_action_capable_enum(1) ->
    "CAPABLE";
map_action_capable_enum(2) ->
    "WAIT";
map_action_capable_enum(Other) ->
    ct:log("Mapping action capable ~p to enum not defined", [Other]),
    "NOT_DEFINED".

    
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

%%%--------------------------------------------------------------------
%%% Description: Netconf
%%%--------------------------------------------------------------------

netconf(F, A) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    case apply(ct_netconfc, F, A) of
    ok ->
        ok = ct_netconfc:close_session(nc1),
        ok;
    {ok, R} ->
        ok = ct_netconfc:close_session(nc1),
        {ok, R};
    {error, Error} ->
        {error, Error}
    end.


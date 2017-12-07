%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lici_erlang_SUITE.erl %
%%% @author etxjovp
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R2A/R3A/4
%%%
%%% @doc Test suite for testing the erlang API of the LICI interface
%%%
%%% @end
%%%-------------------------------------------------------------------



-module(lici_erlang_SUITE).
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
%%% Rev        Date        Name        What
%%% -----      ---------   --------    ------------------------
%%% R1A/5      2012-10-02  etxivri     Added more TCs
%%% R1A/6      2012-10-03  etxivri     Created a group to be executed in jenkins
%%% R1A/6      2012-10-03  etxivri     Removed warnings.
%%% R2A/2      2013-02-28  etxkols     Added rct_core hook
%%% R3A/1      2015-07-10  etxjovp     Add group definitions used by CS CI
%%% R3A/2      2015-07-15  etxjovp     mofify group definitions used by CS CI
%%% ----------------------------------------------------------
%%%

%%-compile(export_all).
-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 groups/0,
	 all/0,
	 init_service/1,
	 feature_subscr/1,
	 feature_unsubscr/1,
	 max_feat_subscr/1,
	 max_feat_unsubscr/1,
	 capacity_subscr/1,
	 max_capa_subscr/1,
	 status_subscr/1,
	 is_LKF_installed/1,
	 is_lkf_installed_notsupp_pv/1,
	 clients_subscribe_to_same_licence/1,
	 max_clients_and_subscr/1,
	 max_clients_and_paralell_subscr/1,
	 loop_feature_subscr_and_unsubscr/1,
	 loop_feature_subscr_and_unsubscr_paralell/1,
	 get_lici_info/1
	]).

-include_lib("common_test/include/ct.hrl").

-define(RPC_CALL(M, F, A), rct_rpc:call(rpc_1, M, F, A, 10000)).
-define(CLIENT(F, A), ?RPC_CALL(lici_erlang_client, F, A)).

%% @hidden
%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {minutes, 600}}, % 10 hours
     {ct_hooks, [{rct_rpc, rpc_1},
		 %{cover_hook,[{du1, username}]},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
		 {rct_htmllink,[]},
                 {rct_core,[]}
		]}].

%% @hidden
%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = [tuple()]
%% Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Module = lici_erlang_client,
    {Module, Binary, Filename} = code:get_object_code(Module),
    case ?RPC_CALL(code, load_binary, [Module, Filename, Binary]) of
	{module, Module} ->
	    Config;
	{error, Reason} ->
	    {skip, Reason}
    end.

%% @hidden
%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = [tuple()]
%% Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%% @hidden
%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = [tuple()]
%% Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%% @hidden
%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = [tuple()]
%% Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Start Client and add it in Config.<br/>
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = [tuple()]
%% Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Client1 = ?RPC_CALL(lici_erlang_client, start, []),
    %% add client to Config
    [{clients,[Client1]}|Config].

%%--------------------------------------------------------------------
%% @doc
%% Stop the Client that was started from init_  .<br/>
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = [tuple()]
%% Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
    Clients = ?config(clients, Config),
    %% [?CLIENT(stop, Pid) || Pid <- Clients],
    lists:foreach(fun(Y)->
    			  ?RPC_CALL(lici_erlang_client, stop, [Y])
    		  end, Clients),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Add all TCs you want to run in seq i the list.<br/>
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [init_service,
     feature_subscr,
     feature_unsubscr,
     max_feat_subscr,
     max_feat_unsubscr,
     capacity_subscr,
     max_capa_subscr,
     status_subscr,
     is_LKF_installed,
     is_lkf_installed_notsupp_pv,
     clients_subscribe_to_same_licence,
     max_clients_and_subscr,
     max_clients_and_paralell_subscr
     %% loop_feature_subscr_and_unsubscr,
     %% loop_feature_subscr_and_unsubscr_paralell
    ].

%%--------------------------------------------------------------------
%% @doc
%% TCs runed by jenkins.
%% @end
%%--------------------------------------------------------------------
groups() ->
    AllGroup = all(),
    [{default__group, [], AllGroup},
    {sbc__qual__all__1__group, [], []},
    {sbc__def__all__1__group, [], [{group, group_lici_erl_1_1}]},  
    {sbc__upgrade__all__1__group, [], [{group, group_lici_erl_1_1}]},  
    {sbc__upgrade_short__all__1__group, [], [{group, group_lici_erl_1_1}]},  
    {sdc__cover__sim__1__group, [], [{group, group_lici_erl_1_1}]},
    {sdc__def__all__1__group, [], [{group, group_lici_erl_1_1}]},  
    {sdc__qual__all__1__group, [], []},
    {group_lici_erl_1, [], [{group, group_lici_erl_1_1}]},  
    {group_lici_erl_1_1,[],[init_service,
			 feature_subscr,
			 feature_unsubscr,
			 max_feat_subscr,
			 max_feat_unsubscr,
			 capacity_subscr,
			 max_capa_subscr,
			 status_subscr,
			 is_LKF_installed,
			 is_lkf_installed_notsupp_pv,
			 clients_subscribe_to_same_licence,
			 max_clients_and_subscr,
			 max_clients_and_paralell_subscr]}].

%%--------------------------------------------------------------------
%% @doc
%% The function is used to request connection to the License Manager Server.<br/>
%% TC will use both invalid and correct protocol verion. <br/>
%% @spec init_service(Config) -> ok
%% @end
%%--------------------------------------------------------------------
init_service(Config) ->
    %% Client = ?RPC_CALL(lici_erlang_client, start, []),
    %% ?CLIENT(start, []),
    [Client| _] = ?config(clients, Config),

    SignalRev=1,
    [] = get_lici_info(subscriptions),
    [_] = get_lici_info(providers),

%% io:get_line("### A Check, press return\r\n"),

    {lici_init_rej, SignalRev, invalid_protocol_version} =
	?RPC_CALL(lici_erlang_client, initiate_service, [Client, [4,4,4]]),
    %% One element in the providers list
    [_] = get_lici_info(providers),

    {lici_init_cfm, Pid, 1, 3} =
	?RPC_CALL(lici_erlang_client, initiate_service, [Client, [0,3,2]]),
    %% Two elements in the providers list
    [_,_] = get_lici_info(providers),

    lici_term_cfm = ?RPC_CALL(lici_erlang_client, terminate_service, [Client, Pid]),
    %% One elements in the providers list
    [_] = get_lici_info(providers),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% The function is used to request subscription to a license controlled feature.<br/>
%% @spec feature_subscr(Config) -> ok
%% @end
%%--------------------------------------------------------------------
feature_subscr(Config) ->
    [] = get_lici_info(subscriptions),
    [_] = get_lici_info(providers),

    SignalRev = 1,
    PV = 3,
    %% FeatureStatus = disabled,
    %% ChangeReason = not_activated,
    FeatureStatus = enabled,
    ChangeReason = licensed_value,
    [Client| _] = ?config(clients, Config),

    {lici_init_cfm, Pid, SignalRev, PV} =
	?RPC_CALL(lici_erlang_client, initiate_service, [Client, [PV]]),

    {lici_feature_subscr_cfm, "ABC_123/4", FeatureStatus, ChangeReason} =
    ?RPC_CALL(lici_erlang_client, feature_subscribe, [Client, Pid, "aBc_123/4"]),

    [_] = get_lici_info(subscriptions),
    ct:pal("one feature_subscr ~p", [get_lici_info(subscriptions)]),

    {lici_feature_subscr_rej, "ABC_123/4", already_subscribed} =
	?RPC_CALL(lici_erlang_client, feature_subscribe, [Client, Pid, "ABC_123/4"]),
    [_] = get_lici_info(subscriptions),

    {lici_feature_subscr_cfm, "ABC123/5", FeatureStatus, ChangeReason} =
	?RPC_CALL(lici_erlang_client, feature_subscribe, [Client, Pid, "ABC123/5"]),

    [_,_] = get_lici_info(subscriptions),
    ct:pal("two feature_subscr ~p", [get_lici_info(subscriptions)]),

    lici_term_cfm = ?RPC_CALL(lici_erlang_client, terminate_service, [Client, Pid]),
    [] = get_lici_info(subscriptions),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% The function is used to request unsubscription of a license controlled feature.<br/>
%% @spec feature_unsubscr(Config) -> ok
%% @end
%%--------------------------------------------------------------------
feature_unsubscr(Config) ->
    [] = get_lici_info(subscriptions),
    [_] = get_lici_info(providers),

    SignalRev = 1,
    PV = 3,
    [Client| _] = ?config(clients, Config),

    {lici_init_cfm, Pid, SignalRev, PV} =
	?RPC_CALL(lici_erlang_client, initiate_service, [Client, [PV]]),

    {lici_feature_unsubscr_cfm, "ABC_123/4"} =
	?RPC_CALL(lici_erlang_client, feature_unsubscribe, [Client, Pid, "aBc_123/4"]),
    [] = get_lici_info(subscriptions),

    {lici_feature_subscr_cfm, "ABC_123/4", _, _} =
	?RPC_CALL(lici_erlang_client, feature_subscribe, [Client, Pid, "aBc_123/4"]),
    [_] = get_lici_info(subscriptions),

    {lici_feature_unsubscr_cfm, "ABC_123/4"} =
	?RPC_CALL(lici_erlang_client, feature_unsubscribe, [Client, Pid, "aBc_123/4"]),
    [] = get_lici_info(subscriptions),

    lici_term_cfm = ?RPC_CALL(lici_erlang_client, terminate_service, [Client, Pid]),
    [] = get_lici_info(subscriptions),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% 800 subsrcriptions to a license controlled feature.
%% @spec max_feat_subscr(Config) -> ok
%% @end
%%--------------------------------------------------------------------
max_feat_subscr(Config) ->
    [] = get_lici_info(subscriptions),
    %% One element in the providers list
    [_] = get_lici_info(providers),

    SignalRev = 1,
    PV = 3,
    [Client| _] = ?config(clients, Config),
    {lici_init_cfm, Pid, SignalRev, PV} =
	?RPC_CALL(lici_erlang_client, initiate_service, [Client, [3,2,1]]),

    SubscrNrList = lists:seq(1,800),
    featuresubscription(Client, Pid, SubscrNrList),
    SubscrList = get_lici_info(subscriptions),
    %% ct:pal("SubscrList: ~p",[SubscrList]),
    800 = length(SubscrList),
    ct:pal("feature subscriptions: ~p",[length(get_lici_info(subscriptions))]),

    %% Subscribe nr 801 shall result in SUBSCRIBE_REJ.
    {lici_feature_subscr_rej, "ABC_123/801", unexpected_error} =
	?RPC_CALL(lici_erlang_client, feature_subscribe, [Client, Pid, "aBc_123/801"]),

    800 = length(SubscrList),

    lici_term_cfm = ?RPC_CALL(lici_erlang_client, terminate_service, [Client, Pid]),
    [_] = get_lici_info(providers),
    [] = get_lici_info(subscriptions),

    ok.

featuresubscription(Client, Pid, SubscrNrList) ->
    IdName="aBc_123/",
    ExpectIdName="ABC_123/", % Lici will change format on the input Id to this!
    lists:foreach(fun(SubscrNr) ->
			  %% ct:pal("featuresubscription:~p",[SubscrNr]),
    			  Id_str = IdName ++ integer_to_list(SubscrNr),
			  Expected_ID_str = ExpectIdName ++ integer_to_list(SubscrNr),
			  {lici_feature_subscr_cfm, Expected_ID_str, _, _} =
			      	?RPC_CALL(lici_erlang_client, feature_subscribe, [Client, Pid, Id_str])
    		  end, SubscrNrList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% 800 unsubscriptions of license cotrolled features.<br/>
%% @spec max_feat_unsubscr(Config) -> ok
%% @end
%%--------------------------------------------------------------------
max_feat_unsubscr(Config) ->
    [] = get_lici_info(subscriptions),
    %% One element in the providers list
    [_] = get_lici_info(providers),

    SignalRev = 1,
    PV = 3,
    [Client| _] = ?config(clients, Config),
    {lici_init_cfm, Pid, SignalRev, PV} =
	?RPC_CALL(lici_erlang_client, initiate_service, [Client, [3,2,1]]),

    SubscrNrList = lists:seq(1,800),
    featuresubscription(Client, Pid, SubscrNrList),
    SubscrList = get_lici_info(subscriptions),
    800 = length(SubscrList),
    ct:pal("feature subscriptions: ~p",[length(get_lici_info(subscriptions))]),

    ct:pal("Unsubscribe all",[]),
    feature_unsubscription(Client, Pid, SubscrNrList),
    [] = get_lici_info(subscriptions),
    ct:pal("feature subscriptions: ~p",[length(get_lici_info(subscriptions))]),

    lici_term_cfm = ?RPC_CALL(lici_erlang_client, terminate_service, [Client, Pid]),
    [_] = get_lici_info(providers),
    [] = get_lici_info(subscriptions),

    ok.

feature_unsubscription(Client, Pid, SubscrNrList) ->
    IdName="aBc_123/",
    ExpectIdName="ABC_123/", % Lici will change format on the input Id to this!
    lists:foreach(fun(SubscrNr) ->
			  %%ct:pal("features_unubscription:~p",[SubscrNr]),
    			  Id_str = IdName ++ integer_to_list(SubscrNr),
			  Expected_ID_str = ExpectIdName ++ integer_to_list(SubscrNr),
			  {lici_feature_unsubscr_cfm, Expected_ID_str} =
			      	?RPC_CALL(lici_erlang_client, feature_unsubscribe, [Client, Pid, Id_str])
    		  end, SubscrNrList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% The function is used to request subscription to a license controlled capacity.<br/>
%% @spec capacity_subscr(Config) -> ok
%% @end
%%--------------------------------------------------------------------
capacity_subscr(Config) ->
    [] = get_lici_info(subscriptions),
    [_] = get_lici_info(providers),

    SignalRev = 1,
    PV = 3,
    %% LicensedLevel = 0,
    %% HardLimit = 0,
    %% ChangeReason = not_activated,
    LicensedLevel = nolimit,
    HardLimit = nolimit,
    ChangeReason = licensed_value,
    [Client| _] = ?config(clients, Config),

    {lici_init_cfm, Pid, SignalRev, PV} =
		?RPC_CALL(lici_erlang_client, initiate_service, [Client, [PV]]),

    {lici_capacity_subscr_cfm, "ABC456/7", LicensedLevel, HardLimit, ChangeReason} =
    ?RPC_CALL(lici_erlang_client, capacity_subscribe, [Client, Pid, "aBc 456/7"]),
    [_] = get_lici_info(subscriptions),
    ct:pal("one capacity_subscr ~p", [get_lici_info(subscriptions)]),

    {lici_capacity_subscr_rej, "ABC456/7", already_subscribed} =
	?RPC_CALL(lici_erlang_client, capacity_subscribe, [Client, Pid, "aBc 456/7"]),
    [_] = get_lici_info(subscriptions),

    {lici_capacity_subscr_cfm, "ABC567/8", _, _, _} =
	?RPC_CALL(lici_erlang_client, capacity_subscribe, [Client, Pid, "ABC567/8"]),
    [_,_] = get_lici_info(subscriptions),
    ct:pal("two capacity_subscr ~p", [get_lici_info(subscriptions)]),

    lici_term_cfm = ?RPC_CALL(lici_erlang_client, terminate_service, [Client, Pid]),
    [] = get_lici_info(subscriptions),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% 800 subscriptions of license cotrolled capacity.<br/>
%% @spec max_capa_subscr(Config) -> ok
%% @end
%%--------------------------------------------------------------------
max_capa_subscr(Config) ->
    [] = get_lici_info(subscriptions),
    %% One element in the providers list
    [_] = get_lici_info(providers),

    SignalRev = 1,
    PV = 3,
    [Client| _] = ?config(clients, Config),

    {lici_init_cfm, Pid, SignalRev, PV} =
	?RPC_CALL(lici_erlang_client, initiate_service, [Client, [3,2,1]]),

    SubscrNrList = lists:seq(1,800),
    capacitysubscriptions(Client, Pid, SubscrNrList),
    SubscrList = get_lici_info(subscriptions),
    %% ct:pal("SubscrList: ~p",[SubscrList]),
    800 = length(SubscrList),
    ct:pal("Capacity subscriptions: ~p", [length(get_lici_info(subscriptions))]),

    %% Subscribe nr 801 shall result in SUBSCRIBE_REJ.
    {lici_capacity_subscr_rej, "ABC_456/801", unexpected_error} =
	?RPC_CALL(lici_erlang_client, capacity_subscribe, [Client, Pid, "aBc_456/801"]),

    800 = length(SubscrList),

    lici_term_cfm = ?RPC_CALL(lici_erlang_client, terminate_service, [Client, Pid]),
    [_] = get_lici_info(providers),
    [] = get_lici_info(subscriptions),

    ok.

capacitysubscriptions(Client, Pid, SubscrNrList) ->
    IdName="aBc_456/",
    ExpectIdName="ABC_456/", % Lici will change format on the input Id to this!
    lists:foreach(fun(SubscrNr) ->
			  %% ct:pal("capacitysubscriptions:~p",[SubscrNr]),
    			  Id_str = IdName ++ integer_to_list(SubscrNr),
			  Expected_ID_str = ExpectIdName ++ integer_to_list(SubscrNr),
			  {lici_capacity_subscr_cfm, Expected_ID_str, _, _, _} =
			      ?RPC_CALL(lici_erlang_client, capacity_subscribe, [Client, Pid, Id_str])
    		  end, SubscrNrList).

%%--------------------------------------------------------------------
%% @doc
%% The function is used to request subscription to a license controlled status.<br/>
%% @spec status_subscr(Config) -> ok
%% @end
%%--------------------------------------------------------------------
status_subscr(Config) ->
    [] = get_lici_info(subscriptions),
    [_] = get_lici_info(providers),

    SignalRev = 1,
    PV = 3,
    LicMgrStatus = deactivated,
    EmergencyCounter = no_emergency,
    [Client| _] = ?config(clients, Config),

    {lici_init_cfm, Pid, SignalRev, PV} =
	?RPC_CALL(lici_erlang_client, initiate_service, [Client, [PV]]),

    {lici_status_subscr_cfm, LicMgrStatus, EmergencyCounter} =
	?RPC_CALL(lici_erlang_client, status_subscribe, [Client, Pid]),
    [_] = get_lici_info(subscriptions),
    ct:pal("one status_subscr ~p", [get_lici_info(subscriptions)]),

    {lici_status_subscr_rej, already_subscribed} =
	?RPC_CALL(lici_erlang_client, status_subscribe, [Client, Pid]),
    [_] = get_lici_info(subscriptions),

    lici_term_cfm = ?RPC_CALL(lici_erlang_client, terminate_service, [Client, Pid]),
    [] = get_lici_info(subscriptions),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% The function is used to check if a License Key file is installed on the license server.<br/>
%% @spec is_LKF_installed(Config) -> ok
%% @end
%%--------------------------------------------------------------------
is_LKF_installed(Config) ->
    [] = get_lici_info(subscriptions),
    [_] = get_lici_info(providers),

    SignalRev = 1,
    PV = 3,
    LFKResult = false,
    [Client| _] = ?config(clients, Config),

    {lici_init_cfm, Pid, SignalRev, PV} =
	?RPC_CALL(lici_erlang_client, initiate_service, [Client, [PV]]),

    {lici_is_lkf_installed_rsp, SignalRev, LFKResult} =
	?RPC_CALL(lici_erlang_client, is_LKF_installed, [Client, Pid]),

    lici_term_cfm = ?RPC_CALL(lici_erlang_client, terminate_service, [Client, Pid]),
    [] = get_lici_info(subscriptions),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% is_lkf_installed_notsupp_pv<br/>
%% is_lkf_installed when selected protocol version does not support the function..
%% @spec is_lkf_installed_notsupp_pv(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
is_lkf_installed_notsupp_pv(Config) ->
    [] = get_lici_info(subscriptions),
    [_] = get_lici_info(providers),

    SignalRev = 1,
    PV = 1,
    [Client| _] = ?config(clients, Config),

    {lici_init_cfm, Pid, SignalRev, PV} =
	?RPC_CALL(lici_erlang_client, initiate_service, [Client, [1,2,1]]),

    {lici_is_lkf_installed_rsp, SignalRev, false} =
	?RPC_CALL(lici_erlang_client, is_LKF_installed, [Client, Pid]),

    lici_term_cfm = ?RPC_CALL(lici_erlang_client, terminate_service, [Client, Pid]),
    [] = get_lici_info(subscriptions),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Check that two clients can subsribe to one particular licence key at same time.<br/>
%% @spec clients_subscribe_to_same_licence(Config) -> ok
%% @end
%%--------------------------------------------------------------------
clients_subscribe_to_same_licence(Config) ->
    [] = get_lici_info(subscriptions),
    [_] = get_lici_info(providers),

    SignalRev = 1,
    PV = 3,
    [Client1| _] = ?config(clients, Config),

    %%%%
    %% Start Client 2 and 3 is done in init_per_siute
    %%%%
    Client2 = ?RPC_CALL(lici_erlang_client, start, []),
    ct:pal("Client2: ~p", [Client2]),
    Client3 = ?RPC_CALL(lici_erlang_client, start, []),
    ct:pal("Client3: ~p", [Client3]),

    %%%%
    %% Initiate service for client 1, 2 and 3
    %%%%
    %% 1
    {lici_init_cfm, Pid1, SignalRev, PV} =
	?RPC_CALL(lici_erlang_client, initiate_service, [Client1, [3,2,1]]),
    %% 2
    {lici_init_cfm, Pid2, SignalRev, PV} =
	?RPC_CALL(lici_erlang_client, initiate_service, [Client2, [3,2,1]]),
    %% 3
    {lici_init_cfm, Pid3, SignalRev, PV} =
	?RPC_CALL(lici_erlang_client, initiate_service, [Client3, [3,2,1]]),
    [_,_,_,_] = get_lici_info(providers),

    %%%%
    %% Feature Subscribe
    %%%%
    %% 1
    {lici_feature_subscr_cfm, "ABC_123/4", _, _} =
	?RPC_CALL(lici_erlang_client, feature_subscribe, [Client1, Pid1, "aBc_123/4"]),
    %% 2
    {lici_feature_subscr_cfm, "ABC_123/4", _, _} =
	?RPC_CALL(lici_erlang_client, feature_subscribe, [Client2, Pid2, "aBc_123/4"]),

    [_] = get_lici_info(subscriptions),
    ct:pal("feature subscriptions: ~p",[get_lici_info(subscriptions)]),
    A= get_lici_info(subscriptions),

    %% REJ when third client wants to subcribe same licence.
    {lici_feature_subscr_rej, "ABC_123/4", unexpected_error} =
	?RPC_CALL(lici_erlang_client, feature_subscribe, [Client3, Pid3, "aBc_123/4"]),
    %% Check that nothing is changed.
    B = get_lici_info(subscriptions),
    A=B,

    %%%%
    %% Capacity Subscribe
    %%%%
    %% 1
    {lici_capacity_subscr_cfm, "ABC_456/7", _, _, _} =
	?RPC_CALL(lici_erlang_client, capacity_subscribe, [Client1, Pid1, "aBc_456/7"]),
    %% 2
    {lici_capacity_subscr_cfm, "ABC_456/7", _, _, _} =
	?RPC_CALL(lici_erlang_client, capacity_subscribe, [Client2, Pid2, "aBc_456/7"]),

    [_,_] = get_lici_info(subscriptions),
    ct:pal("feature and capacity subscriptions : ~p",[get_lici_info(subscriptions)]),
    C = get_lici_info(subscriptions),

    %% REJ when third client wants to subcribe same licence.
    {lici_capacity_subscr_rej, "ABC_456/7", unexpected_error} =
	?RPC_CALL(lici_erlang_client, capacity_subscribe, [Client3, Pid3, "aBc_456/7"]),
    %% Check that nothing is changed.
    D = get_lici_info(subscriptions),
    C=D,


    %%%%
    %% Terminate Service
    %%%%
    lici_term_cfm = ?RPC_CALL(lici_erlang_client, terminate_service, [Client1, Pid1]),
    lici_term_cfm = ?RPC_CALL(lici_erlang_client, terminate_service, [Client2, Pid2]),
    lici_term_cfm = ?RPC_CALL(lici_erlang_client, terminate_service, [Client3, Pid3]),
    [] = get_lici_info(subscriptions),
    [_] = get_lici_info(providers),

    %%%%
    %% Stop Clinets 2 and 3
    %%%%
    ?RPC_CALL(lici_erlang_client, stop, [Client2]),
    ?RPC_CALL(lici_erlang_client, stop, [Client3]),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% The server can serve max 100 clients. Perform also subscr and check is_lkf_<br/>
%% Also check that 101 clients result in INITIATE_SERVICE_REJ.<br/>
%% - Check that max status subscription is possible, 1 per client.<br/>
%% - mix also capability and feature subscriptions.<br/>
%% - check also is_lkf_installed<br/>
%% - This TC will checl the answer.<br/>
%% @spec max_clients_and_subscr(Config) -> ok
%% @end
%%--------------------------------------------------------------------
max_clients_and_subscr(Config) ->
    [] = get_lici_info(subscriptions),
    %% One element in the providers list
    [_] = get_lici_info(providers),

    SignalRev = 1,
    PV = 3,
    [Client1| _] = ?config(clients, Config),

    %%%%
    %% Start Client 2 to 100
    %%%%
    NrOfClients = lists:seq(2,100),
    ClientList = lists:map(fun(_Client) ->
				?RPC_CALL(lici_erlang_client, start, [])
			end, NrOfClients),
    CompleteClientList = [Client1|ClientList],
    100 = length(CompleteClientList),
    ct:pal("100 clients, started.", []),

    %%%%
    %% Initiate service for client 1 to 100
    %%%%
    PidList = lists:map(fun(Client) ->
				{lici_init_cfm, Pid, SignalRev, PV} =
				       ?RPC_CALL(lici_erlang_client, initiate_service, [Client, [PV]]),
				%ct:pal("Nr: ~p", [Client]),
				Pid
		  end, CompleteClientList),
    100 = length(PidList),
    101 = length(get_lici_info(providers)),
    ct:pal("initiate service for each client, done.", []),

    %% Check that 101 initiate service result in INITIATE_SERVICE_REJ
    Client101 = ?RPC_CALL(lici_erlang_client, start, []),
    {lici_init_rej, SignalRev, unexpected_error} =
	?RPC_CALL(lici_erlang_client, initiate_service, [Client101, [3,1,2]]),

    %%%%
    %% Terminate a service Client51 and create a new on Client101.
    %%%%
    Client51 = lists:nth(51, CompleteClientList),
    Pid51 = lists:nth(51, PidList),
    lici_term_cfm = ?RPC_CALL(lici_erlang_client, terminate_service, [Client51, Pid51]),
    {lici_init_cfm, NewPid, SignalRev, PV} =
	?RPC_CALL(lici_erlang_client, initiate_service, [Client101, [3,1,2]]),

    ct:pal("terminate a service and initiate a new service on new Client, done.", []),

    ?RPC_CALL(lici_erlang_client, stop, [Client51]),
    %% Fix the client and Pid List
    A_ClientList = lists:delete(Client51, CompleteClientList),
    99 = length(A_ClientList),
    NewClientList = lists:append(A_ClientList, [Client101]),
    100 = length(NewClientList),
    A_PidList = lists:delete(Pid51, PidList),
    99 = length(A_PidList),
    NewPidList = lists:append(A_PidList, [NewPid]),
    100 = length(NewPidList),


    Clients_Pids_List = lists:zip(NewClientList, NewPidList), % makes a touple list of two list.

    %%%%
    %% Do 800 subscriptions, mixed
    %%%%
    lists:foldl(fun({Client, Pid}, Nr) ->
			featuresubscription(Client, Pid, [Nr]),
			Nr+1
		  end, 1, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			capacitysubscriptions(Client, Pid, [Nr]),
			Nr+1
		  end, 1, Clients_Pids_List),
    ct:pal("100 featuresubscription and 100 capacitysubscriptions, done.", []),
    lists:foldl(fun({Client, Pid}, Nr) ->
			featuresubscription(Client, Pid, [Nr]),
			Nr+1
		  end, 101, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			capacitysubscriptions(Client, Pid, [Nr]),
			Nr+1
		  end, 101, Clients_Pids_List),
    ct:pal("200 featuresubscription and 200 capacitysubscriptions, done.", []),
    lists:foldl(fun({Client, Pid}, Nr) ->
			featuresubscription(Client, Pid, [Nr]),
			Nr+1
		  end, 201, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			capacitysubscriptions(Client, Pid, [Nr]),
			Nr+1
		  end, 201, Clients_Pids_List),
    ct:pal("300 featuresubscription and 300 capacitysubscriptions, done.", []),
    lists:foldl(fun({Client, Pid}, Nr) ->
			featuresubscription(Client, Pid, [Nr]),
			Nr+1
		  end, 301, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			capacitysubscriptions(Client, Pid, [Nr]),
			Nr+1
		  end, 301, Clients_Pids_List),
    ct:pal("400 featuresubscription and 400 capacitysubscriptions, done.", []),
    800=length(get_lici_info(subscriptions)),

    %%%%
    %% Status subscription on every client
    %%%%
    lists:foreach(fun({Client, Pid}) ->
			  {lici_status_subscr_cfm, _LicMgrStatus, _EmergencyCounter} =
			      ?RPC_CALL(lici_erlang_client, status_subscribe, [Client, Pid])
		  end, Clients_Pids_List),
    801=length(get_lici_info(subscriptions)),
    ct:pal("100 status subscription, done.", []),


    %%%%
    %% check isLKFInstalled on every client
    %%%%
    lists:foreach(fun({Client, Pid}) ->
			  {lici_is_lkf_installed_rsp, SignalRev, _LFKResult} =
			      ?RPC_CALL(lici_erlang_client, is_LKF_installed, [Client, Pid])
		  end, Clients_Pids_List),
    ct:pal("100 isLKFInstalled, done.", []),

    101 = length(get_lici_info(providers)),

    %% io:get_line("### Check, press return\r\n"),

    %%%%
    %% Terminate Service
    %%%%
    lists:foreach(fun({Client, Pid}) ->
			  lici_term_cfm = ?RPC_CALL(lici_erlang_client, terminate_service, [Client, Pid])
		  end, Clients_Pids_List),

    [] = get_lici_info(subscriptions),
    [_] = get_lici_info(providers),

    %%%%
    %% Stop Clients 2 to 100
    %%%%
    [_H | ClientTailList] = NewClientList, %% get 2 to 100
    lists:foreach(fun(Client) ->
			  ?RPC_CALL(lici_erlang_client, stop, [Client])
		  end, ClientTailList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% The server can serve max 100 clients. Perform also paralell subscr and check is_lkf_<br/>
%% - Check that max status subscription is possible, 1 per client.<br/>
%% - mix also capability and feature subscriptions.<br/>
%% - check also is_lkf_installed<br/>
%% No check on the return value<br/>
%% @spec max_clients_and_paralell_subscr(Config) -> ok
%% @end
%%--------------------------------------------------------------------
max_clients_and_paralell_subscr(Config) ->
    [] = get_lici_info(subscriptions),
    %% One element in the providers list
    [_] = get_lici_info(providers),

    SignalRev = 1,
    PV = 3,
    [Client1| _] = ?config(clients, Config),

    %%%%
    %% Start Client 2 to 100
    %%%%
    NrOfClients = lists:seq(2,100),
    ClientList = lists:map(fun(_Client) ->
				?RPC_CALL(lici_erlang_client, start, [])
			end, NrOfClients),
    CompleteClientList = [Client1|ClientList],
    100 = length(CompleteClientList),
    ct:pal("100 clients, started.", []),

    %%%%
    %% Initiate service for client 1 to 100
    %%%%
    PidList = lists:map(fun(Client) ->
				{lici_init_cfm, Pid, SignalRev, PV} =
				       ?RPC_CALL(lici_erlang_client, initiate_service, [Client, [PV]]),
				%ct:pal("Nr: ~p", [Client]),
				Pid
		  end, CompleteClientList),
    100 = length(PidList),
    101=length(get_lici_info(providers)),
    ct:pal("initiate service for each client, done.", []),


    Clients_Pids_List = lists:zip(CompleteClientList, PidList), % makes a touple list of two list.

    %%%%
    %% Do 800 subscriptions, mixed
    %%%%
    IdName_A="aBc_123/",
    IdName_B="aBc_456/",
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName_A ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, feature_subscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 1, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName_B ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, capacity_subscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 1, Clients_Pids_List),
    ct:pal("100 featuresubscription and 100 capacitysubscriptions, done.", []),
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName_A ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, feature_subscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 101, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName_B ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, capacity_subscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 101, Clients_Pids_List),
    ct:pal("200 featuresubscription and 200 capacitysubscriptions, done.", []),
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName_A ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, feature_subscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 201, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName_B ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, capacity_subscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 201, Clients_Pids_List),
    ct:pal("300 featuresubscription and 300 capacitysubscriptions, done.", []),
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName_A ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, feature_subscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 301, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName_B ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, capacity_subscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 301, Clients_Pids_List),
    ct:pal("400 featuresubscription and 400 capacitysubscriptions, done.", []),

    800=length(get_lici_info(subscriptions)),

    %%%%
    %% Status subscription on every client
    %%%%
    lists:foreach(fun({Client, Pid}) ->
			      ?RPC_CALL(lici_erlang_client, status_subscribe, [Client, Pid])
		  end, Clients_Pids_List),
    801=length(get_lici_info(subscriptions)),
    ct:pal("100 status subscription, done.", []),

    %%%%
    %% check isLKFInstalled on every client
    %%%%
    lists:foreach(fun({Client, Pid}) ->
			      ?RPC_CALL(lici_erlang_client, is_LKF_installed, [Client, Pid])
		  end, Clients_Pids_List),
    ct:pal("100 isLKFInstalled, done.", []),

    101 = length(get_lici_info(providers)),

    %% io:get_line("### A Check, press return\r\n"),

    %%%%
    %% Terminate Service
    %%%%
    lists:foreach(fun({Client, Pid}) ->
			  lici_term_cfm = ?RPC_CALL(lici_erlang_client, terminate_service, [Client, Pid])
		  end, Clients_Pids_List),

    [] = get_lici_info(subscriptions),
    [_] = get_lici_info(providers),

    %%%%
    %% Stop Clients 2 to 100
    %%%%
    lists:foreach(fun(Client) ->
			  ?RPC_CALL(lici_erlang_client, stop, [Client])
		  end, ClientList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% loop_feature_subscr_and_unsubscr<br/>
%% Use 100 clients,<br/>
%% Check on the return Value<br/>
%% @spec loop_feature_subscr_and_unsubscr(Config) -> ok
%% @end
%%--------------------------------------------------------------------
loop_feature_subscr_and_unsubscr(Config) ->
    [] = get_lici_info(subscriptions),
    %% One element in the providers list
    [_] = get_lici_info(providers),

    SignalRev = 1,
    PV = 3,
    [Client1| _] = ?config(clients, Config),

    %%%%
    %% Start Client 2 to 100
    %%%%
    NrOfClients = lists:seq(2,100),
    ClientList = lists:map(fun(_Client) ->
				?RPC_CALL(lici_erlang_client, start, [])
			end, NrOfClients),
    CompleteClientList = [Client1|ClientList],
    100 = length(CompleteClientList),
    ct:pal("100 clients, started.", []),

    %%%%
    %% Initiate service for client 1 to 100
    %%%%
    PidList = lists:map(fun(Client) ->
				{lici_init_cfm, Pid, SignalRev, PV} =
				       ?RPC_CALL(lici_erlang_client, initiate_service, [Client, [PV]]),
				%ct:pal("Nr: ~p", [Client]),
				Pid
		  end, CompleteClientList),
    100 = length(PidList),
    101=length(get_lici_info(providers)),
    ct:pal("initiate service for each client, done.", []),

    Clients_Pids_List = lists:zip(CompleteClientList, PidList), % makes a touple list of two list.

    LoopNr = lists:seq(1,500),
    lists:foreach(fun(_X) ->
			 %% ct:pal("Start LoopNr: ~p", [X]),
    %%%%
    %% Do 800 subscriptions.
    %%%%
    lists:foldl(fun({Client, Pid}, Nr) ->
			featuresubscription(Client, Pid, [Nr]),
			Nr+1
		  end, 1, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			featuresubscription(Client, Pid, [Nr]),
			Nr+1
		  end, 101, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			featuresubscription(Client, Pid, [Nr]),
			Nr+1
		  end, 201, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			featuresubscription(Client, Pid, [Nr]),
			Nr+1
		  end, 301, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			featuresubscription(Client, Pid, [Nr]),
			Nr+1
		  end, 401, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			featuresubscription(Client, Pid, [Nr]),
			Nr+1
		  end, 501, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			featuresubscription(Client, Pid, [Nr]),
			Nr+1
		  end, 601, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			featuresubscription(Client, Pid, [Nr]),
			Nr+1
		  end, 701, Clients_Pids_List),
    %% ct:pal("800 featuresubscription, done.", []),

    800=length(get_lici_info(subscriptions)),

    %% io:get_line("### A Check, press return\r\n"),

    %%%%
    %% Do 800 unsubscriptions.
    %%%%
    lists:foldl(fun({Client, Pid}, Nr) ->
			feature_unsubscription(Client, Pid, [Nr]),
			Nr+1
		  end, 1, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			feature_unsubscription(Client, Pid, [Nr]),
			Nr+1
		  end, 101, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			feature_unsubscription(Client, Pid, [Nr]),
			Nr+1
		  end, 201, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			feature_unsubscription(Client, Pid, [Nr]),
			Nr+1
		  end, 301, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			feature_unsubscription(Client, Pid, [Nr]),
			Nr+1
		  end, 401, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			feature_unsubscription(Client, Pid, [Nr]),
			Nr+1
		  end, 501, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			feature_unsubscription(Client, Pid, [Nr]),
			Nr+1
		  end, 601, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			feature_unsubscription(Client, Pid, [Nr]),
			Nr+1
		  end, 701, Clients_Pids_List),
    %% ct:pal("800 features_unubscription, done.", []),

    0 =length(get_lici_info(subscriptions))

			 %% ct:pal("End loopNr: ~p",[X])
			 end, LoopNr),

    ct:pal("LoopNr: ~p, done",[length(LoopNr)]),

    %%%%
    %% Terminate Service
    %%%%
    lists:foreach(fun({Client, Pid}) ->
			  lici_term_cfm = ?RPC_CALL(lici_erlang_client, terminate_service, [Client, Pid])
		  end, Clients_Pids_List),

    [] = get_lici_info(subscriptions),
    [_] = get_lici_info(providers),

    %%%%
    %% Stop Clients 2 to 100
    %%%%
    lists:foreach(fun(Client) ->
			  ?RPC_CALL(lici_erlang_client, stop, [Client])
		  end, ClientList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% loop_feature_subscr_and_unsubscr_paralell<br/>
%% Use 100 clients,<br/>
%% No check on the return value<br/>
%% @spec loop_feature_subscr_and_unsubscr_paralell(Config) -> ok
%% @end
%%--------------------------------------------------------------------
loop_feature_subscr_and_unsubscr_paralell(Config) ->
    [] = get_lici_info(subscriptions),
    %% One element in the providers list
    [_] = get_lici_info(providers),

    SignalRev = 1,
    PV = 3,
    [Client1| _] = ?config(clients, Config),

    %%%%
    %% Start Client 2 to 100
    %%%%
    NrOfClients = lists:seq(2,100),
    ClientList = lists:map(fun(_Client) ->
				?RPC_CALL(lici_erlang_client, start, [])
			end, NrOfClients),
    CompleteClientList = [Client1|ClientList],
    100 = length(CompleteClientList),
    ct:pal("100 clients, started.", []),

    %%%%
    %% Initiate service for client 1 to 100
    %%%%
    PidList = lists:map(fun(Client) ->
				{lici_init_cfm, Pid, SignalRev, PV} =
				       ?RPC_CALL(lici_erlang_client, initiate_service, [Client, [PV]]),
				%ct:pal("Nr: ~p", [Client]),
				Pid
		  end, CompleteClientList),
    100 = length(PidList),
    101=length(get_lici_info(providers)),
    ct:pal("initiate service for each client, done.", []),


    Clients_Pids_List = lists:zip(CompleteClientList, PidList), % makes a touple list of two list.
    IdName="aBc_123/",

    LoopNr = lists:seq(1,500),
    %% ct:pal("LoopNr: ~p", [LoopNr]),

    lists:foreach(fun(_X) ->
			 %% ct:pal("Start LoopNr: ~p", [X]),
    %%%%
    %% Do 800 subscriptions.
    %%%%
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, feature_subscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 1, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, feature_subscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 101, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, feature_subscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 201, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, feature_subscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 301, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, feature_subscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 401, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, feature_subscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 501, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, feature_subscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 601, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, feature_subscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 701, Clients_Pids_List),
    %% ct:pal("800 featuresubscription, no check on return value, done.", []),

    800=length(get_lici_info(subscriptions)),

    %% io:get_line("### A Check, press return\r\n"),

    %%%%
    %% Do 800 unsubscriptions.
    %%%%
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, feature_unsubscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 1, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, feature_unsubscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 101, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, feature_unsubscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 201, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, feature_unsubscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 301, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, feature_unsubscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 401, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, feature_unsubscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 501, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, feature_unsubscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 601, Clients_Pids_List),
    lists:foldl(fun({Client, Pid}, Nr) ->
			Id_str = IdName ++ integer_to_list(Nr),
			?RPC_CALL(lici_erlang_client, feature_unsubscribe, [Client, Pid, Id_str]),
			Nr+1
		  end, 701, Clients_Pids_List),
    %% ct:pal("800 features_unubscription, no check on return value, done.", []),

    0 =length(get_lici_info(subscriptions))

			 %% ct:pal("End loopNr: ~p",[X])
			 end, LoopNr),

    ct:pal("LoopNr: ~p, done",[length(LoopNr)]),

    %%%%
    %% Terminate Service
    %%%%
    lists:foreach(fun({Client, Pid}) ->
			  lici_term_cfm = ?RPC_CALL(lici_erlang_client, terminate_service, [Client, Pid])
		  end, Clients_Pids_List),

    [] = get_lici_info(subscriptions),
    [_] = get_lici_info(providers),

    %%%%
    %% Stop Clients 2 to 100
    %%%%
    lists:foreach(fun(Client) ->
			  ?RPC_CALL(lici_erlang_client, stop, [Client])
		  end, ClientList),

    ok.

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
get_lici_info(Type) ->
    List = ?RPC_CALL(lih_lici_server, get_info, []),
    proplists:get_value(Type, List).

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lici_c_SUITE.erl %
%%% @author etxjovp
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R2A/R3A/5
%%%
%%% @doc ==Basic Test Suite for testing the lici legacy c interface interface on SUT.==
%%% This Test Suite can be used on both target and simulated enviroment.
%%% <br/><br/>
%%% rct_proxy is used in ct_hooks:
%%% see <a href="file:///vobs/rcs/test/RCT_CRX901275/test/lib/rct_proxy/esrc/rct_proxy.erl">rct_proxy.erl</a><br/>
%%%
%%% @end


-module(lici_c_SUITE).
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
%%% R1A/3      2012-09-03  etxivri     Created
%%% R1A/4      2012-09-10  etxivri     Added more TCs
%%% R1A/9      2012-10-03  etxivri     Created a group to be executed in jenkins
%%% R2A/2      2013-02-28  etxkols     Added rct_core hook
%%% R2A/3      2013-04-17  etxjovp     change timetrap to 30
%%% R3A/1      2015-07-10  etxjovp     Add group definitions used by CS CI
%%% R3A/2      2015-07-15  etxjovp     modify group definitions used by CS CI
%%% ----------------------------------------------------------
%%%

%-compile(export_all).
-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 init_per_group/2,
	 end_per_group/2,
	 groups/0,
	 all/0,
	 initiate_service/1,
	 terminate_service/0,
	 incor_prot_vers/1,
	 cor_prot_vers/1,
	 feat_subscr/1,
	 feat_subscr_unavail/1,
	 max_feat_subscr/1,
	 capa_subscr/1,
	 capa_subscr_unavail/1,
	 max_capa_subscr/1,
	 status_subscr/1,
	 status_subscr_unavail/1,
	 is_lkf_installed/1,
	 is_lkf_installed_unavail/1,
	 is_lkf_installed_notsupp_pv/1,
	 clients_subscribe_to_same_licence/1,
	 max_clients_and_subscr/1,
	 max_clients_and_paralell_subscr/1
	 %loop/1
	]).

-include_lib("common_test/include/ct.hrl").
-include("test_lici.hrl").

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_proxy set up enviroment so to handle comunication with application on SUT. <br/>
%% @end
%%--------------------------------------------------------------------
suite() ->
   [{timetrap, {minutes, 30}},
    {ct_hooks, [{rct_htmllink,[]},
		{rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
		{rct_proxy,[{1, node1, ssh_lmt_ipv4, du1, username}]},
		{rct_core,[]}
	       ]}].


%% @hidden
init_per_suite(Config) ->
    {ok, client_started} =
	rct_proxy:start_proxy(node1, lici1, ?LICI),
    {ok, memory_initiated} =
    	rct_proxy:send_proxy(node1, lici1, ?CelloLici_initiateMemory),
    Config.


%% @hidden
end_per_suite(_Config) ->
    timer:sleep(2000),
    {ok, client_stopped} = rct_proxy:stop_proxy(node1, lici1),
    ok = rct_proxy:exit_master(node1),
    ok.


%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.


%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.

%% @hidden
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [incor_prot_vers,
     cor_prot_vers,
     feat_subscr,
     feat_subscr_unavail,
     max_feat_subscr,
     capa_subscr,
     capa_subscr_unavail,
     max_capa_subscr,
     status_subscr,
     status_subscr_unavail,
     is_lkf_installed,
     is_lkf_installed_unavail,
     is_lkf_installed_notsupp_pv,
     clients_subscribe_to_same_licence,
     max_clients_and_subscr
     %max_clients_and_paralell_subscr
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
    {sbc__def__all__1__group, [], [{group, group_lici_c_1_1}]},  
    {sbc__upgrade__all__1__group, [], [{group, group_lici_c_1_1}]},  
    {sbc__upgrade_short__all__1__group, [], [{group, group_lici_c_1_1}]},  
    {sdc__cover__sim__1__group, [], [{group, group_lici_c_1_1}]},
    {sdc__def__all__1__group, [], [{group, group_lici_c_1_1}]},  
    {sdc__qual__all__1__group, [], []},
    {group_lici_c_1, [], [{group, group_lici_c_1_1}]},
    {group_lici_c_1_1,[],[incor_prot_vers,
		       cor_prot_vers,
		       feat_subscr,
		       feat_subscr_unavail,
		       max_feat_subscr,
		       capa_subscr,
		       capa_subscr_unavail,
		       max_capa_subscr,
		       status_subscr,
		       status_subscr_unavail,
		       is_lkf_installed,
		       is_lkf_installed_unavail,
		       is_lkf_installed_notsupp_pv,
		       clients_subscribe_to_same_licence]}].

%%--------------------------------------------------------------------
%% @doc
%% The function is used to request connection to the License Manager Server, using incorrect protocolls.<br/>
%% @spec incor_prot_vers(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
incor_prot_vers(_) ->
    {?CELLO_LICI_INITIATE_SERVICE_REJ, 1, 3,
     ?CELLO_LICI_INVALID_PROTOCOL_VERSION} = initiate_service({4, 4, 4}),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% The function is used to request connection to the License Manager Server, using correct protocolls.<br/>
%% @spec cor_prot_vers(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cor_prot_vers(_) ->
    {?CELLO_LICI_INITIATE_SERVICE_CFM, 1, 3} = initiate_service({3, 2, 1}),
    {?CELLO_LICI_TERMINATE_SERVICE_CFM} = terminate_service(),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% The function is used to request subscription to a license controlled feature.<br/>
%% @spec feat_subscr(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
feat_subscr(_) ->
    {?CELLO_LICI_INITIATE_SERVICE_CFM, 1, 3} = initiate_service({3, 2, 1}),

    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici1, ?CelloLici_featureSubscription,
			     {'aBc_123/4'}),
    %% {ok, {?CELLO_LICI_FEATURE_SUBSCRIBE_CFM,
    %% 	  "ABC_123/4",
    %% 	  ?CELLO_LICI_FEATURE_DISABLED,
    %% 	  ?CELLO_LICI_NOT_ACTIVATED}} = rct_proxy:receive_proxy(),
    {ok, {?CELLO_LICI_FEATURE_SUBSCRIBE_CFM,
	  "ABC_123/4",
	  ?CELLO_LICI_FEATURE_ENABLED,
	  ?CELLO_LICI_LICENSED_VALUE}} = rct_proxy:receive_proxy(),

    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici1, ?CelloLici_featureSubscription,
			     {'aBc_123/4'}),
    {ok, {?CELLO_LICI_FEATURE_SUBSCRIBE_REJ,
	  "ABC_123/4",
	  ?CELLO_LICI_ALREADY_SUBSCRIBED}} = rct_proxy:receive_proxy(),

    {?CELLO_LICI_TERMINATE_SERVICE_CFM} = terminate_service(),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% feat_subscr_unavail
%% feature subscription when server is not available.
%% @spec feat_subscr_unavail(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
feat_subscr_unavail(_) ->
    {ok, ?CELLO_LICI_SERVICE_UNAVAIL} =
	rct_proxy:send_proxy(node1, lici1, ?CelloLici_featureSubscription,
			     {'aBc_123/4'}),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% 800 subsrcriptions to a license controlled feature.
%% max is 800 subscriptions for all clients
%% @spec max_feat_subscr(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
max_feat_subscr(_) ->
    {?CELLO_LICI_INITIATE_SERVICE_CFM, 1, 3} = initiate_service({3, 2, 1}),
    SubscrNrList = lists:seq(1,800),
    featuresubscription(SubscrNrList),

    %% Subscribe nr 801 shall result in SUBSCRIBE_REJ.
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici1, ?CelloLici_featureSubscription,
			     {'aBc_123/801'}),
    {ok, {?CELLO_LICI_FEATURE_SUBSCRIBE_REJ,
	  "ABC_123/801",
	  ?CELLO_LICI_UNEXPECTED_ERROR}} = rct_proxy:receive_proxy(),

    {?CELLO_LICI_TERMINATE_SERVICE_CFM} = terminate_service(),

    ok.

featuresubscription(SubscrNrList) ->
    IdName="aBc_123/",
    ExpectIdName="ABC_123/", % Lici will change format on the input Id to this!
    lists:foreach(fun(SubscrNr) ->
			  %% ct:pal("featuresubscription:~p",[SubscrNr]),
    			  Id_atom = list_to_atom(IdName ++ integer_to_list(SubscrNr)),
			  {ok, ?CELLO_LICI_SUCCESS} =
			      rct_proxy:send_proxy(node1, lici1, ?CelloLici_featureSubscription, {Id_atom}),
			  Expected_ID_str = ExpectIdName ++ integer_to_list(SubscrNr),
			  %% {ok, {?CELLO_LICI_FEATURE_SUBSCRIBE_CFM,
			  %% 	Expected_ID_str,
			  %% 	?CELLO_LICI_FEATURE_DISABLED,
			  %% 	?CELLO_LICI_NOT_ACTIVATED}} = rct_proxy:receive_proxy()
			  {ok, {?CELLO_LICI_FEATURE_SUBSCRIBE_CFM,
				Expected_ID_str,
				?CELLO_LICI_FEATURE_ENABLED,
				?CELLO_LICI_LICENSED_VALUE}} = rct_proxy:receive_proxy()
    		  end, SubscrNrList).

featuresubscription(Client, ClientNr_SubscrNr_List) ->
    IdName="aBc_123/",
    ExpectIdName="ABC_123/", % Lici will change format on the input Id to this!
    lists:foreach(fun({ClientNr, SubscrNr}) ->
			  Client_Nr = list_to_atom(Client ++ integer_to_list(ClientNr)),
    			  Id_atom = list_to_atom(IdName ++ integer_to_list(SubscrNr)),
			  %% ct:pal("featuresubscription:~p, ~p",[Client_Nr,SubscrNr]),
			  {ok, ?CELLO_LICI_SUCCESS} =
			      rct_proxy:send_proxy(node1, Client_Nr, ?CelloLici_featureSubscription, {Id_atom}),
			  Expected_ID_str = ExpectIdName ++ integer_to_list(SubscrNr),
			  %% {ok, {?CELLO_LICI_FEATURE_SUBSCRIBE_CFM,
			  %% 	Expected_ID_str,
			  %% 	?CELLO_LICI_FEATURE_DISABLED,
			  %% 	?CELLO_LICI_NOT_ACTIVATED}} = rct_proxy:receive_proxy()
			  {ok, {?CELLO_LICI_FEATURE_SUBSCRIBE_CFM,
				Expected_ID_str,
				?CELLO_LICI_FEATURE_ENABLED,
				?CELLO_LICI_LICENSED_VALUE}} = rct_proxy:receive_proxy()
    		  end, ClientNr_SubscrNr_List).


%% %%--------------------------------------------------------------------
%% loop(_) ->
%%     Nr =lists:seq(1,100),
%%     lists:foreach(fun(Y) ->
%% 			  ct:pal("start: ~p", [Y]),
%% 			  {?CELLO_LICI_INITIATE_SERVICE_CFM, 1, 3} = initiate_service({3, 2, 1}),
%% 			  SubscrNrList = lists:seq(1,800),
%% 			  featuresubscription(SubscrNrList),

%% 			  {?CELLO_LICI_TERMINATE_SERVICE_CFM} = terminate_service(),
%% 			  ct:pal("End: ~p", [Y])

%% 		  end, Nr).


%%--------------------------------------------------------------------
%% @doc
%% The function is used to request subscription to a license controlled capacity.<br/>
%% @spec capa_subscr(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
capa_subscr(_) ->
    {?CELLO_LICI_INITIATE_SERVICE_CFM, 1, 3} = initiate_service({3, 2, 1}),

    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici1, ?CelloLici_capacitySubscription,
			     {'aBc_456/1'}),
    %% {ok, {?CELLO_LICI_CAPACITY_SUBSCRIBE_CFM,
    %% 	  "ABC_456/1",
    %% 	  ?CELLO_LICI_LICENSED_LEVEL_VALUE_VALID,0,
    %% 	  ?CELLO_LICI_HARD_LIMIT_VALUE_VALID,0,
    %% 	  ?CELLO_LICI_NOT_ACTIVATED}} = rct_proxy:receive_proxy(),
    {ok, {?CELLO_LICI_CAPACITY_SUBSCRIBE_CFM,
	  "ABC_456/1",
	  ?CELLO_LICI_LICENSED_LEVEL_VALUE_NOT_VALID,0,
	  ?CELLO_LICI_HARD_LIMIT_NOT_VALID,0,
	  ?CELLO_LICI_LICENSED_VALUE}} = rct_proxy:receive_proxy(),

    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici1, ?CelloLici_capacitySubscription,
			     {'aBc_456/1'}),
    {ok, {?CELLO_LICI_CAPACITY_SUBSCRIBE_REJ,
	  "ABC_456/1",
	  ?CELLO_LICI_ALREADY_SUBSCRIBED}} = rct_proxy:receive_proxy(),

    {?CELLO_LICI_TERMINATE_SERVICE_CFM} = terminate_service(),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% capa_subscr_unavail<br/>
%% capacity subscription when server is not available.
%% @spec capa_subscr_unavail(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
capa_subscr_unavail(_) ->
    {ok, ?CELLO_LICI_SERVICE_UNAVAIL} =
    	rct_proxy:send_proxy(node1, lici1, ?CelloLici_capacitySubscription,
    			    {'aBc_456/1'}),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% 800 subscriptions of license cotrolled capacity.<br/>
%% max is 800 subscriptions for all clients
%% @spec max_capa_subscr(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
max_capa_subscr(_) ->
    {?CELLO_LICI_INITIATE_SERVICE_CFM, 1, 3} = initiate_service({3, 2, 1}),
    SubscrNrList =lists:seq(1,800),
    capacitysubscriptions(SubscrNrList),

    %% Subscribe nr 801 shall result in SUBSCRIBE_REJ.
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici1, ?CelloLici_capacitySubscription,
			     {'aBc_456/801'}),
    {ok, {?CELLO_LICI_CAPACITY_SUBSCRIBE_REJ,
	  "ABC_456/801",
	  ?CELLO_LICI_UNEXPECTED_ERROR}} = rct_proxy:receive_proxy(),


    {?CELLO_LICI_TERMINATE_SERVICE_CFM} = terminate_service(),

    ok.

capacitysubscriptions(SubscrNrList) ->
    IdName="aBc_456/",
    ExpectIdName="ABC_456/", % Lici will change format on the input Id to this!
    lists:foreach(fun(SubscrNr) ->
			  %ct:pal("capacitysubscriptions:~p",[SubscrNr]),
    			  Id_atom = list_to_atom(IdName ++ integer_to_list(SubscrNr)),
			  {ok, ?CELLO_LICI_SUCCESS} =
			      rct_proxy:send_proxy(node1, lici1, ?CelloLici_capacitySubscription,
						   {Id_atom}),
			  Expected_ID_str = ExpectIdName ++ integer_to_list(SubscrNr),
			  %% {ok, {?CELLO_LICI_CAPACITY_SUBSCRIBE_CFM,
			  %% 	Expected_ID_str,
			  %% 	?CELLO_LICI_LICENSED_LEVEL_VALUE_VALID,0,
			  %% 	?CELLO_LICI_HARD_LIMIT_VALUE_VALID,0,
			  %% 	?CELLO_LICI_NOT_ACTIVATED}} = rct_proxy:receive_proxy()
			  {ok, {?CELLO_LICI_CAPACITY_SUBSCRIBE_CFM,
			  	Expected_ID_str,
			  	?CELLO_LICI_LICENSED_LEVEL_VALUE_NOT_VALID,0,
			  	?CELLO_LICI_HARD_LIMIT_NOT_VALID,0,
			  	?CELLO_LICI_LICENSED_VALUE}} = rct_proxy:receive_proxy()
    		  end, SubscrNrList).

capacitysubscriptions(Client, ClientNr_SubscrNr_List) ->
    IdName="aBc_456/",
    ExpectIdName="ABC_456/", % Lici will change format on the input Id to this!
    lists:foreach(fun({ClientNr, SubscrNr}) ->
			  Client_Nr = list_to_atom(Client ++ integer_to_list(ClientNr)),
    			  Id_atom = list_to_atom(IdName ++ integer_to_list(SubscrNr)),
			  %ct:pal("capacitysubscriptions: ~p, ~p",[Client_Nr, SubscrNr]),
			  {ok, ?CELLO_LICI_SUCCESS} =
			      rct_proxy:send_proxy(node1, Client_Nr, ?CelloLici_capacitySubscription,
						   {Id_atom}),
			  Expected_ID_str = ExpectIdName ++ integer_to_list(SubscrNr),
			  %% {ok, {?CELLO_LICI_CAPACITY_SUBSCRIBE_CFM,
			  %% 	Expected_ID_str,
			  %% 	?CELLO_LICI_LICENSED_LEVEL_VALUE_VALID,0,
			  %% 	?CELLO_LICI_HARD_LIMIT_VALUE_VALID,0,
			  %% 	?CELLO_LICI_NOT_ACTIVATED}} = rct_proxy:receive_proxy()
			  {ok, {?CELLO_LICI_CAPACITY_SUBSCRIBE_CFM,
			  	Expected_ID_str,
			  	?CELLO_LICI_LICENSED_LEVEL_VALUE_NOT_VALID,0,
				?CELLO_LICI_HARD_LIMIT_NOT_VALID,0,
				?CELLO_LICI_LICENSED_VALUE}} = rct_proxy:receive_proxy()
    		  end, ClientNr_SubscrNr_List).


%%--------------------------------------------------------------------
%% @doc
%% The function is used to request subscription to a license controlled status.<br/>
%% @spec status_subscr(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
status_subscr(_) ->
    {?CELLO_LICI_INITIATE_SERVICE_CFM, 1, 3} = initiate_service({3, 2, 1}),

    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici1, ?CelloLici_statusSubscription),
    {ok, {?CELLO_LICI_STATUS_SUBSCRIBE_CFM,
	  ?CELLO_LICI_EMERGENCY_DEACTIVATED,
	  ?CELLO_LICI_NO_EMERGENCY}} = rct_proxy:receive_proxy(),

    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici1, ?CelloLici_statusSubscription),
    {ok, {?CELLO_LICI_STATUS_SUBSCRIBE_REJ,
	  ?CELLO_LICI_ALREADY_SUBSCRIBED}} = rct_proxy:receive_proxy(),

    {?CELLO_LICI_TERMINATE_SERVICE_CFM} = terminate_service(),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% status_subscr_unavail
%% status subscription when server is not available.
%% @spec status_subscr_unavail(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
status_subscr_unavail(_) ->
    {ok, ?CELLO_LICI_SERVICE_UNAVAIL} =
    	rct_proxy:send_proxy(node1, lici1, ?CelloLici_statusSubscription),

    ok.


%% @hidden
%%--------------------------------------------------------------------
%% @doc
%% Use several clients
%% @end
%%--------------------------------------------------------------------
%% Use to status description in max client TC
statussubscription(Client, ClientNrList) ->
    lists:foreach(fun(ClientNr) ->
			  Client_Nr = list_to_atom(Client ++ integer_to_list(ClientNr)),
			  %ct:pal("statussubscription:~p",[Client_Nr]),
			  {ok, ?CELLO_LICI_SUCCESS} =
			      rct_proxy:send_proxy(node1, Client_Nr, ?CelloLici_statusSubscription),
			  {ok, {?CELLO_LICI_STATUS_SUBSCRIBE_CFM,
				?CELLO_LICI_EMERGENCY_DEACTIVATED,
				?CELLO_LICI_NO_EMERGENCY}} = rct_proxy:receive_proxy()
    		  end, ClientNrList).

%%--------------------------------------------------------------------
%% @doc
%% The function is used to check if a License Key file is installed on the license server.<br/>
%% @spec is_lkf_installed(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
is_lkf_installed(_) ->
    {?CELLO_LICI_INITIATE_SERVICE_CFM, 1, 3} = initiate_service({3, 2, 1}),

    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici1, ?CelloLici_isLKFInstalled),
    {ok, {?CELLO_LICI_IS_LKF_INSTALLED_RSP, 1, 1}} = rct_proxy:receive_proxy(),

    {?CELLO_LICI_TERMINATE_SERVICE_CFM} = terminate_service(),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% is_lkf_installed<br/>
%% is_lkf_installed when server is not available.
%% @spec is_lkf_installed_unavail(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
is_lkf_installed_unavail(_) ->
    {ok, ?CELLO_LICI_SERVICE_UNAVAIL} =
    	rct_proxy:send_proxy(node1, lici1, ?CelloLici_isLKFInstalled),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% is_lkf_installed when selected protocol version does not support the function <br/>
%% @spec is_lkf_installed_notsupp_pv(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
is_lkf_installed_notsupp_pv(_) ->
    {?CELLO_LICI_INITIATE_SERVICE_CFM, 1, 1} = initiate_service({1, 2, 1}),

    {ok, ?CELLO_LICI_NOT_SUPPORTED_BY_SELECTED_PV} =
    	rct_proxy:send_proxy(node1, lici1, ?CelloLici_isLKFInstalled),

    {?CELLO_LICI_TERMINATE_SERVICE_CFM} = terminate_service(),

    ok.

%% @hidden
%%--------------------------------------------------------------------
%% @doc
%% Use several clients
%% @end
%%--------------------------------------------------------------------
%% Use in max client TC
isLKFInstalled(Client, ClientNrList) ->
    lists:foreach(fun(ClientNr) ->
			  Client_Nr = list_to_atom(Client ++ integer_to_list(ClientNr)),
			  %ct:pal("isLKFInstalled:~p",[Client_Nr]),
			  {ok, ?CELLO_LICI_SUCCESS} =
			      rct_proxy:send_proxy(node1, Client_Nr, ?CelloLici_isLKFInstalled),
			  {ok, {?CELLO_LICI_IS_LKF_INSTALLED_RSP, 1, ?CELLO_LICI_LKF_NOT_INSTALLED}} =
			      rct_proxy:receive_proxy()
    		  end, ClientNrList).

%%--------------------------------------------------------------------
%% @doc
%% Check that two clients can subsribe to one particular licence key at same time.<br/>
%% @spec clients_subscribe_to_same_licence(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
clients_subscribe_to_same_licence(_) ->
    ProtocolVersion = {3, 2, 1},

    %%%%
    %% Start Client 2 and 3.
    %%%%
    %% 2
    {ok, client_started} =
	rct_proxy:start_proxy(node1, lici2, ?LICI),
    {ok, memory_initiated} =
	rct_proxy:send_proxy(node1, lici2, ?CelloLici_initiateMemory),
    %% 3
    {ok, client_started} =
	rct_proxy:start_proxy(node1, lici3, ?LICI),
    {ok, memory_initiated} =
	rct_proxy:send_proxy(node1, lici3, ?CelloLici_initiateMemory),
    %%%%
    %% Initiate service for client 1, 2 and 3
    %%%%
    %% 1
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici1, ?CelloLici_initiateService,
			     ProtocolVersion),
    {ok, {?CELLO_LICI_SERVER_UP_IND}} = rct_proxy:receive_proxy(),
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici1, ?CelloLici_internal),
    {ok, {?CELLO_LICI_INITIATE_SERVICE_CFM,1,3}} = rct_proxy:receive_proxy(),
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici1, ?CelloLici_internal),
    %% 2
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici2, ?CelloLici_initiateService,
			     ProtocolVersion),
    {ok, {?CELLO_LICI_SERVER_UP_IND}} = rct_proxy:receive_proxy(),
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici2, ?CelloLici_internal),
    {ok, {?CELLO_LICI_INITIATE_SERVICE_CFM,1,3}} = rct_proxy:receive_proxy(),
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici2, ?CelloLici_internal),
    %% 3
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici3, ?CelloLici_initiateService,
			     ProtocolVersion),
    {ok, {?CELLO_LICI_SERVER_UP_IND}} = rct_proxy:receive_proxy(),
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici3, ?CelloLici_internal),
    {ok, {?CELLO_LICI_INITIATE_SERVICE_CFM,1,3}} = rct_proxy:receive_proxy(),
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici3, ?CelloLici_internal),

    %%%%
    %% Feature Subscribe
    %%%%
    %% 1
    {ok, ?CELLO_LICI_SUCCESS} =
    	rct_proxy:send_proxy(node1, lici1, ?CelloLici_featureSubscription,
    			     {'aBc_123/4'}),
    %% {ok, {?CELLO_LICI_FEATURE_SUBSCRIBE_CFM,
    %% 	  "ABC_123/4",
    %% 	  ?CELLO_LICI_FEATURE_DISABLED,
    %% 	  ?CELLO_LICI_NOT_ACTIVATED}} = rct_proxy:receive_proxy(),
    {ok, {?CELLO_LICI_FEATURE_SUBSCRIBE_CFM,
    	  "ABC_123/4",
    	  ?CELLO_LICI_FEATURE_ENABLED,
    	  ?CELLO_LICI_LICENSED_VALUE}} = rct_proxy:receive_proxy(),
    %% 2
    {ok, ?CELLO_LICI_SUCCESS} =
    	rct_proxy:send_proxy(node1, lici2, ?CelloLici_featureSubscription,
    			     {'aBc_123/4'}),
    %% {ok, {?CELLO_LICI_FEATURE_SUBSCRIBE_CFM,
    %% 	  "ABC_123/4",
    %% 	  ?CELLO_LICI_FEATURE_DISABLED,
    %% 	  ?CELLO_LICI_NOT_ACTIVATED}} = rct_proxy:receive_proxy(),
    {ok, {?CELLO_LICI_FEATURE_SUBSCRIBE_CFM,
    	  "ABC_123/4",
    	  ?CELLO_LICI_FEATURE_ENABLED,
    	  ?CELLO_LICI_LICENSED_VALUE}} = rct_proxy:receive_proxy(),

    %% REJ when third client wants to subcribe same licence.
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici3, ?CelloLici_featureSubscription,
			     {'aBc_123/4'}),
    {ok, {?CELLO_LICI_FEATURE_SUBSCRIBE_REJ,
	  "ABC_123/4",
	  ?CELLO_LICI_UNEXPECTED_ERROR}} = rct_proxy:receive_proxy(),


    %%%%
    %% Capacity Subscribe
    %%%%
    %% 1
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici1, ?CelloLici_capacitySubscription,
			     {'aBc_456/1'}),
    %% {ok, {?CELLO_LICI_CAPACITY_SUBSCRIBE_CFM,
    %% 	  "ABC_456/1",
    %% 	  ?CELLO_LICI_LICENSED_LEVEL_VALUE_VALID,0,
    %% 	  ?CELLO_LICI_HARD_LIMIT_VALUE_VALID,0,
    %% 	  ?CELLO_LICI_NOT_ACTIVATED}} = rct_proxy:receive_proxy(),
    {ok, {?CELLO_LICI_CAPACITY_SUBSCRIBE_CFM,
	  "ABC_456/1",
	  ?CELLO_LICI_LICENSED_LEVEL_VALUE_NOT_VALID,0,
	  ?CELLO_LICI_HARD_LIMIT_NOT_VALID,0,
	  ?CELLO_LICI_LICENSED_VALUE}} = rct_proxy:receive_proxy(),
    %% 2
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici2, ?CelloLici_capacitySubscription,
			     {'aBc_456/1'}),
    %% {ok, {?CELLO_LICI_CAPACITY_SUBSCRIBE_CFM,
    %% 	  "ABC_456/1",
    %% 	  ?CELLO_LICI_LICENSED_LEVEL_VALUE_VALID,0,
    %% 	  ?CELLO_LICI_HARD_LIMIT_VALUE_VALID,0,
    %% 	  ?CELLO_LICI_NOT_ACTIVATED}} = rct_proxy:receive_proxy(),
    {ok, {?CELLO_LICI_CAPACITY_SUBSCRIBE_CFM,
	  "ABC_456/1",
	  ?CELLO_LICI_LICENSED_LEVEL_VALUE_NOT_VALID,0,
	  ?CELLO_LICI_HARD_LIMIT_NOT_VALID,0,
	  ?CELLO_LICI_LICENSED_VALUE}} = rct_proxy:receive_proxy(),

    %% REJ when third client wants to subcribe same licence.
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici3, ?CelloLici_capacitySubscription,
			     {'aBc_456/1'}),
    {ok, {?CELLO_LICI_CAPACITY_SUBSCRIBE_REJ,
	  "ABC_456/1", ?CELLO_LICI_UNEXPECTED_ERROR}} = rct_proxy:receive_proxy(),

    %%%%
    %% Terminate Service
    %%%%
    %% 1
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici1, ?CelloLici_terminateService),
    {ok, {?CELLO_LICI_TERMINATE_SERVICE_CFM}} = rct_proxy:receive_proxy(),
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici1, ?CelloLici_internal),
    %% 2
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici2, ?CelloLici_terminateService),
    {ok, {?CELLO_LICI_TERMINATE_SERVICE_CFM}} = rct_proxy:receive_proxy(),
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici2, ?CelloLici_internal),
    %% 3
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici3, ?CelloLici_terminateService),
    {ok, {?CELLO_LICI_TERMINATE_SERVICE_CFM}} = rct_proxy:receive_proxy(),
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici3, ?CelloLici_internal),

    %% io:get_line("### Check, press return\r\n"),
    {ok, client_stopped} = rct_proxy:stop_proxy(node1, lici2),
    {ok, client_stopped} = rct_proxy:stop_proxy(node1, lici3),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% The server can serve max 100 clients. Perform also subscr and check is_lkf_<br/>
%% The server can serve max 100 clients, according to FS 155 17-CNX 102 30 Uen<br/>
%% Also check that 101 clients result in INITIATE_SERVICE_REJ.<br/>
%% - Check that max status subscription is possible, 1 per client.
%% - mix also capability and feature subscriptions.
%% - check also is_lkf_installed
%% @spec max_clients_and_subscr(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
max_clients_and_subscr(_) ->
    Client= "lici",
    Nr_to_Start = lists:seq(2,100), % lici1 is already started from init_per_suite

    %%  Start 100 clients and initiate memory
    lists:foreach(fun(X) ->
    			  Client_Nr = list_to_atom(Client ++ integer_to_list(X)),
    			  {ok, client_started} =
			      rct_proxy:start_proxy(node1, Client_Nr, ?LICI),
    			  {ok, memory_initiated} =
    			      rct_proxy:send_proxy(node1, Client_Nr, ?CelloLici_initiateMemory)
    		  end, Nr_to_Start),

    %io:get_line("### A Check, press return\r\n"),

    %% Initiate service
    ProtocolVersion = {3, 2, 1},
    Nr =lists:seq(1,100),
    lists:foreach(fun(Y) ->
			  ClientNr = list_to_atom(Client ++ integer_to_list(Y)),
			  {ok, ?CELLO_LICI_SUCCESS} =
			      rct_proxy:send_proxy(node1, ClientNr, ?CelloLici_initiateService,
						   ProtocolVersion),
			  {ok, {?CELLO_LICI_SERVER_UP_IND}} = rct_proxy:receive_proxy(),

			  {ok, ?CELLO_LICI_SUCCESS} =
			      rct_proxy:send_proxy(node1, ClientNr, ?CelloLici_internal),

			  {ok, {?CELLO_LICI_INITIATE_SERVICE_CFM,1,3}} = rct_proxy:receive_proxy(),
			  {ok, ?CELLO_LICI_SUCCESS} =
			      rct_proxy:send_proxy(node1, ClientNr, ?CelloLici_internal)

			  %% ct:pal("Nr: ~p", [Y]),
			  %% io:get_line("### B Check, press return\r\n")

		  end, Nr),
   %% io:get_line("### B Check, press return\r\n"),

    %% Check that 101 clients result in INITIATE_SERVICE_REJ
    {ok, client_started} = rct_proxy:start_proxy(node1, lici101, ?LICI),
    {ok, memory_initiated} = rct_proxy:send_proxy(node1, lici101, ?CelloLici_initiateMemory),

    %% Initaiate service shall result in REJ with reason Unecpected Error.
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici101, ?CelloLici_initiateService,
			     ProtocolVersion),
    {ok, {?CELLO_LICI_SERVER_UP_IND}} = rct_proxy:receive_proxy(),
    {ok, ?CELLO_LICI_SUCCESS} = rct_proxy:send_proxy(node1, lici101, ?CelloLici_internal),
    {ok, {?CELLO_LICI_INITIATE_SERVICE_REJ,1,3,?CELLO_LICI_UNEXPECTED_ERROR}} =
	rct_proxy:receive_proxy(),
    {ok, ?CELLO_LICI_SUCCESS} = rct_proxy:send_proxy(node1, lici101, ?CelloLici_internal),

    %%%%
    %% Terminate a service lici51 and create a new on lici101.
    %%%%
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici51, ?CelloLici_terminateService),
    {ok, {?CELLO_LICI_TERMINATE_SERVICE_CFM}} = rct_proxy:receive_proxy(),
    {ok, ?CELLO_LICI_SUCCESS} =
			      rct_proxy:send_proxy(node1, lici51, ?CelloLici_internal),
    %% Initiate a new service on lici101.
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici101, ?CelloLici_initiateService,
			     ProtocolVersion),
    {ok, {?CELLO_LICI_SERVER_UP_IND}} = rct_proxy:receive_proxy(),
    {ok, ?CELLO_LICI_SUCCESS} = rct_proxy:send_proxy(node1, lici101, ?CelloLici_internal),
    {ok, {?CELLO_LICI_INITIATE_SERVICE_CFM,1,3}} = rct_proxy:receive_proxy(),
    {ok, ?CELLO_LICI_SUCCESS} = rct_proxy:send_proxy(node1, lici101, ?CelloLici_internal),

    %%%%
    %% Terminate service on lici101 and create on 51 again.
    %%%%
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici101, ?CelloLici_terminateService),
    {ok, {?CELLO_LICI_TERMINATE_SERVICE_CFM}} = rct_proxy:receive_proxy(),
    {ok, ?CELLO_LICI_SUCCESS} =
			      rct_proxy:send_proxy(node1, lici101, ?CelloLici_internal),
    %% Initiate a new service on lici51.
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici51, ?CelloLici_initiateService,
			     ProtocolVersion),
    {ok, {?CELLO_LICI_SERVER_UP_IND}} = rct_proxy:receive_proxy(),
    {ok, ?CELLO_LICI_SUCCESS} = rct_proxy:send_proxy(node1, lici51, ?CelloLici_internal),
    {ok, {?CELLO_LICI_INITIATE_SERVICE_CFM,1,3}} = rct_proxy:receive_proxy(),
    {ok, ?CELLO_LICI_SUCCESS} = rct_proxy:send_proxy(node1, lici51, ?CelloLici_internal),

    %%%%
    %% Do 800 subscriptions, mixed
    %%%%
    ClientNrList=lists:seq(1,100),
    SubscrNr_1=lists:seq(1,100),
    ClientNr_SubscrNr_List_1 = lists:zip(ClientNrList, SubscrNr_1), % makes a touple list of two list.
    featuresubscription(Client, ClientNr_SubscrNr_List_1),
    capacitysubscriptions(Client, ClientNr_SubscrNr_List_1),

    SubscrNr_2=lists:seq(101,200),
    ClientNr_SubscrNr_List_2 = lists:zip(ClientNrList, SubscrNr_2),
    featuresubscription(Client, ClientNr_SubscrNr_List_2),
    capacitysubscriptions(Client, ClientNr_SubscrNr_List_2),

    SubscrNr_3=lists:seq(201,300),
    ClientNr_SubscrNr_List_3 = lists:zip(ClientNrList, SubscrNr_3),
    featuresubscription(Client, ClientNr_SubscrNr_List_3),
    capacitysubscriptions(Client, ClientNr_SubscrNr_List_3),

    SubscrNr_4=lists:seq(301,400),
    ClientNr_SubscrNr_List_4 = lists:zip(ClientNrList, SubscrNr_4),
    featuresubscription(Client, ClientNr_SubscrNr_List_4),
    capacitysubscriptions(Client, ClientNr_SubscrNr_List_4),

    %%%%
    %% Status subscription on every client
    %%%%
    statussubscription(Client, ClientNrList),

    %%%%
    %% check isLKFInstalled on every client
    %%%%
    isLKFInstalled(Client, ClientNrList),

    %% io:get_line("### B Check, press return\r\n"),

    %%%%
    %%  Terminate all services
    %%%%
    lists:foreach(fun(Z) ->
			  TerminateClientNr = list_to_atom(Client ++ integer_to_list(Z)),
			  {ok, ?CELLO_LICI_SUCCESS} =
			      rct_proxy:send_proxy(node1, TerminateClientNr, ?CelloLici_terminateService),
			  {ok, {?CELLO_LICI_TERMINATE_SERVICE_CFM}} = rct_proxy:receive_proxy(),
			  {ok, ?CELLO_LICI_SUCCESS} =
			      rct_proxy:send_proxy(node1, TerminateClientNr, ?CelloLici_internal)
		  end, Nr),

    %% terminate service for lici101 shall result in SERVICE_UNAVAIL since it already terminated.
    {ok, ?CELLO_LICI_SERVICE_UNAVAIL} = rct_proxy:send_proxy(node1, lici101, ?CelloLici_terminateService),

    %%%%
    %%  Stop clients
    %%%%
    lists:foreach(fun(Z) ->
			  TerminateClientNr = list_to_atom(Client ++ integer_to_list(Z)),
			  {ok, client_stopped} = rct_proxy:stop_proxy(node1, TerminateClientNr)
		  end, Nr_to_Start),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% The server can serve max 100 clients. Perform also paralell subscr and check is_lkf_<br/>
%% The server can serve max 100 clients.<br/>
%% Also check that 101 clients result in INITIATE_SERVICE_REJ.<br/>
%% Use 100 clients,<br/>
%% No check on the return value<br/>
%% @spec max_clients_and_paralell_subscr(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
max_clients_and_paralell_subscr(_) ->
    Client= "lici",
    Nr_to_Start = lists:seq(2,100), % lici1 is already started from init_per_suite

    %%  Start 100 clients and initiate memory
    lists:foreach(fun(X) ->
    			  Client_Nr = list_to_atom(Client ++ integer_to_list(X)),
    			  {ok, client_started} =
			      rct_proxy:start_proxy(node1, Client_Nr, ?LICI),
    			  {ok, memory_initiated} =
    			      rct_proxy:send_proxy(node1, Client_Nr, ?CelloLici_initiateMemory)
    		  end, Nr_to_Start),

    %% io:get_line("### A Check, press return\r\n"),

    %% Initiate service
    ProtocolVersion = {3, 2, 1},
    Nr =lists:seq(1,100),
    lists:foreach(fun(Y) ->
			  ClientNr = list_to_atom(Client ++ integer_to_list(Y)),
			  {ok, ?CELLO_LICI_SUCCESS} =
			      rct_proxy:send_proxy(node1, ClientNr, ?CelloLici_initiateService,
						   ProtocolVersion),
			  {ok, {?CELLO_LICI_SERVER_UP_IND}} = rct_proxy:receive_proxy(),

			  {ok, ?CELLO_LICI_SUCCESS} =
			      rct_proxy:send_proxy(node1, ClientNr, ?CelloLici_internal),

			  {ok, {?CELLO_LICI_INITIATE_SERVICE_CFM,1,3}} = rct_proxy:receive_proxy(),
			  {ok, ?CELLO_LICI_SUCCESS} =
			      rct_proxy:send_proxy(node1, ClientNr, ?CelloLici_internal)

			  %% ct:pal("Nr: ~p", [Y]),
			  %% io:get_line("### B Check, press return\r\n")

		  end, Nr),
   %% io:get_line("### B Check, press return\r\n"),

    ClientNrList=lists:seq(1,100),
    SubscrNrList_1=lists:seq(1,100),
    ClientNr_SubscrNr_List_1 = lists:zip(ClientNrList, SubscrNrList_1),
    SubscrNrList_2=lists:seq(101,200),
    ClientNr_SubscrNr_List_2 = lists:zip(ClientNrList, SubscrNrList_2),
    SubscrNrList_3=lists:seq(201,300),
    ClientNr_SubscrNr_List_3 = lists:zip(ClientNrList, SubscrNrList_3),
    SubscrNrList_4=lists:seq(301,400),
    ClientNr_SubscrNr_List_4 = lists:zip(ClientNrList, SubscrNrList_4),

    IdName_1="aBc_123/",
    IdName_2 ="aBc_456/",
    lists:foreach(fun({ClientNr, SubscrNr}) ->
			  Client_Nr = list_to_atom(Client ++ integer_to_list(ClientNr)),
    			  Id_atom = list_to_atom(IdName_1 ++ integer_to_list(SubscrNr)),
			  rct_proxy:send_proxy(node1, Client_Nr, ?CelloLici_featureSubscription, {Id_atom})
    		  end, ClientNr_SubscrNr_List_1),
    lists:foreach(fun({ClientNr, SubscrNr}) ->
			  Client_Nr = list_to_atom(Client ++ integer_to_list(ClientNr)),
    			  Id_atom = list_to_atom(IdName_2 ++ integer_to_list(SubscrNr)),
			  rct_proxy:send_proxy(node1, Client_Nr, ?CelloLici_capacitySubscription,{Id_atom})
    		  end, ClientNr_SubscrNr_List_1),

    lists:foreach(fun({ClientNr, SubscrNr}) ->
			  Client_Nr = list_to_atom(Client ++ integer_to_list(ClientNr)),
    			  Id_atom = list_to_atom(IdName_1 ++ integer_to_list(SubscrNr)),
			  rct_proxy:send_proxy(node1, Client_Nr, ?CelloLici_featureSubscription, {Id_atom})
    		  end, ClientNr_SubscrNr_List_2),
        lists:foreach(fun({ClientNr, SubscrNr}) ->
			  Client_Nr = list_to_atom(Client ++ integer_to_list(ClientNr)),
    			  Id_atom = list_to_atom(IdName_2 ++ integer_to_list(SubscrNr)),
			  rct_proxy:send_proxy(node1, Client_Nr, ?CelloLici_capacitySubscription,{Id_atom})
    		  end, ClientNr_SubscrNr_List_2),

    lists:foreach(fun({ClientNr, SubscrNr}) ->
			  Client_Nr = list_to_atom(Client ++ integer_to_list(ClientNr)),
    			  Id_atom = list_to_atom(IdName_1 ++ integer_to_list(SubscrNr)),
			  rct_proxy:send_proxy(node1, Client_Nr, ?CelloLici_featureSubscription, {Id_atom})
    		  end, ClientNr_SubscrNr_List_3),
    lists:foreach(fun({ClientNr, SubscrNr}) ->
			  Client_Nr = list_to_atom(Client ++ integer_to_list(ClientNr)),
    			  Id_atom = list_to_atom(IdName_2 ++ integer_to_list(SubscrNr)),
			  rct_proxy:send_proxy(node1, Client_Nr, ?CelloLici_capacitySubscription,{Id_atom})
    		  end, ClientNr_SubscrNr_List_3),

    lists:foreach(fun({ClientNr, SubscrNr}) ->
			  Client_Nr = list_to_atom(Client ++ integer_to_list(ClientNr)),
    			  Id_atom = list_to_atom(IdName_1 ++ integer_to_list(SubscrNr)),
			  rct_proxy:send_proxy(node1, Client_Nr, ?CelloLici_featureSubscription, {Id_atom})
    		  end, ClientNr_SubscrNr_List_4),
    lists:foreach(fun({ClientNr, SubscrNr}) ->
			  Client_Nr = list_to_atom(Client ++ integer_to_list(ClientNr)),
			  Id_atom = list_to_atom(IdName_2 ++ integer_to_list(SubscrNr)),
			  rct_proxy:send_proxy(node1, Client_Nr, ?CelloLici_featureSubscription, {Id_atom})
    		  end, ClientNr_SubscrNr_List_4),

    %%%%
    %% Status subscription on every client
    %%%%
    lists:foreach(fun(ClientNr) ->
			  Client_Nr = list_to_atom(Client ++ integer_to_list(ClientNr)),
			      rct_proxy:send_proxy(node1, Client_Nr, ?CelloLici_statusSubscription)
    		  end, ClientNrList),

    %%%%
    %% check isLKFInstalled on every client
    %%%%
    lists:foreach(fun(ClientNr) ->
			  Client_Nr = list_to_atom(Client ++ integer_to_list(ClientNr)),
			      rct_proxy:send_proxy(node1, Client_Nr, ?CelloLici_isLKFInstalled)
    		  end, ClientNrList),

    %% io:get_line("### BB Check, press return\r\n"),

    %%%%
    %%  Terminate all services
    %%%%
    lists:foreach(fun(Z) ->
			  TerminateClientNr = list_to_atom(Client ++ integer_to_list(Z)),
			  rct_proxy:send_proxy(node1, TerminateClientNr, ?CelloLici_terminateService),
			  rct_proxy:send_proxy(node1, TerminateClientNr, ?CelloLici_internal)
		  end, Nr),

    %%%%
    %%  Stop clients
    %%%%
    lists:foreach(fun(Z) ->
			  TerminateClientNr = list_to_atom(Client ++ integer_to_list(Z)),
			  rct_proxy:stop_proxy(node1, TerminateClientNr)
		  end, Nr_to_Start),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% initiate_service
%%
%% @spec initiate_service(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
initiate_service(ProtocolVersion) ->
    case rct_proxy:send_proxy(node1, lici1, ?CelloLici_terminateService) of
	{ok, ?CELLO_LICI_SUCCESS} ->
	    {ok, _} = rct_proxy:receive_proxy(),
	    {ok, ?CELLO_LICI_SUCCESS} =
		rct_proxy:send_proxy(node1, lici1, ?CelloLici_internal);
	_ ->
	    ok
    end,

    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici1, ?CelloLici_initiateService,
			     ProtocolVersion),
    {ok, {?CELLO_LICI_SERVER_UP_IND}} = rct_proxy:receive_proxy(),

    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici1, ?CelloLici_internal),
    {ok, Answer} = rct_proxy:receive_proxy(),
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici1, ?CelloLici_internal),

    Answer.


%%--------------------------------------------------------------------
%% @doc
%% terminate_service
%%
%% @spec terminate_service() -> ok
%% @end
%%--------------------------------------------------------------------
terminate_service() ->
    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici1, ?CelloLici_terminateService),
    {ok, Answer} = rct_proxy:receive_proxy(),

    {ok, ?CELLO_LICI_SUCCESS} =
	rct_proxy:send_proxy(node1, lici1, ?CelloLici_internal),

    Answer.

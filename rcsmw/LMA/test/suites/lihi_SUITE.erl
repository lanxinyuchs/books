%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lihi_SUITE.erl %
%%% @author etxpejn
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R7A/R8A/R9A/1
%%%
%%% @doc Test suite for testing the license feature control interface of LIHI.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(lihi_SUITE).
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
%%% R2A/1      2014-05-06  etxpejn     Moved from RCT to LMA
%%% R4A/8      2015-07-14  etxjovp     Add group definitions used by CS CI
%%% R5A/3      2016-01-28  etxpejn     LicenseGpActivatedFwd moved to lma_basic_SUITE.erl
%%%                                    Added license_feature_control_interface_v2
%%% R5A/4      2016-02-10  etxpejn     Added check for AlarmCorrelationEventId
%%% R5A/7      2016-06-21  etxkols     GIT migration
%%% R7A/1      2016-10-11  etxpejn     Adaptet to new LKF - RCS_MSR_161004_081623.xml
%%% R8A/1      2016-12-13  etxkols     Removing rs232 hook due to cloud
%%% R8A/2      2017-01-13  etxpejn     Updated end_per_suite
%%% R8A/3      2017-02-06  etxpejn     Added license_capacity_control_interface_v2
%%% R9A/1      2017-03-02  etxpejn     Added license_capacity_control_interface_v2 to sim
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
	 license_feature_control_interface/1,
	 license_feature_control_interface_v2/1,
	 license_capacity_control_interface/1,
	 license_capacity_control_interface_v2/1
	]).

-include_lib("common_test/include/ct.hrl").
-include_lib("lihi.hrl").


%% @hidden
%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {minutes, 600}}, % 10 hours
     {ct_hooks, [{rct_rpc, rpc_1},
		 {rct_cli, {cli, [manual_connect]}},
		 {rct_coli, {coli, [manual_connect]}},
		 {rct_logging, {license, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
		 {rct_htmllink,[]},
		 {rct_netconf, nc1},
		 {rct_proxy,[{1, node1, ssh_lmt_ipv4, du1, username}]},
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
    {ok, client_started} = rct_proxy:start_proxy(node1, lihi1, ?LIHI),
    ok = rct_rpc:call(rpc_1, lmaGlms, change_test_mode, [on], 100000, noprint),
    Config.

%% @hidden
%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = [tuple()]
%% Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    rct_proxy:stop_proxy(node1, lihi1),
    rct_proxy:exit_master(node1),
    rct_rpc:call(rpc_1, lmaGlms, change_test_mode, [off], 100000, noprint),
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
    Config.

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
end_per_testcase(_TestCase, _Config) ->
    rct_cli:disconnect(cli, noprint),
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
    [{group, lihi_test}].

%%--------------------------------------------------------------------
%% @doc
%% TCs runed by jenkins.
%% @end
%%--------------------------------------------------------------------
groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},  
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},  
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},  
     {sdc__qual__all__1__group, [], []},  
     {lihi_test, [sequence],
     [
      license_capacity_control_interface,
      license_capacity_control_interface_v2,
      license_feature_control_interface,
      license_feature_control_interface_v2
     ]},
     %% This suite should only be run on core MP within a sim cluster
     {sbc__cluster__dual_sim_1__1__group, [], [{group, default__group}]}
    ].

%%--------------------------------------------------------------------
%% @doc
%% Testcases for the license feature control interface of LIHI.<br/>
%% @spec license_feature_control_interface(Config) -> ok
%% @end
%%--------------------------------------------------------------------
license_feature_control_interface(_Config) ->
    ok = rct_cli:connect(cli),

    IncorrectProtocolVestion = 4,
    ct:pal("Send LfciConnToServerReq to GLMS over LIHI with incorrect protocol version: ~p", 
    	   [IncorrectProtocolVestion]),
     {ok,conn_to_server_req} = rct_proxy:send_proxy(node1, lihi1, ?LfciConnToServerReq, 
						    {IncorrectProtocolVestion}),
    ct:pal("Wait for LfciConnToServerRej from GLMS"),
    {ok, {?LfciConnToServerRej}} = rct_proxy:receive_proxy(),

    ProtocolVersion = 1,
    ct:pal("Send LfciConnToServerReq to GLMS over LIHI with protocol version: ~p", 
	   [ProtocolVersion]),
    {ok,conn_to_server_req} = rct_proxy:send_proxy(node1, lihi1, ?LfciConnToServerReq, 
						   {ProtocolVersion, ?ClientRef}),

    ct:pal("Wait for LfciConnToServerCfm from GLMS"),
    {ok, {?LfciConnToServerCfm, ProtocolVersion, ServerRef}} = rct_proxy:receive_proxy(),
    ct:pal("Connection to GLMS established over LIHI"),
    ct:pal("ServerRef from GLMS in connToServerCfm: ~p", [ServerRef]),

    %% The license user subscribes for license controlled features
    ct:pal("Send LfciFeatureLicenseSubscribeReq to GLMS over LIHI"),
    {ok, feature_license_subscribe_req} = 
	rct_proxy:send_proxy(node1, lihi1, ?LfciFeatureLicenseSubscribeReq, 
			     {ServerRef, ?ClientRef,"CXC4020007", "Name of the feature", 
			      "Description of the feature"}),
    
    ct:pal("Wait for LfciFeatureLicenseSubscribeCfm from GLMS"),
    {ok, {?LfciFeatureLicenseSubscribeCfm, ?ClientRef}} = rct_proxy:receive_proxy(),

    case lmaTestLib:check_if_fingerprint_updateable() of
 	ok ->
	    ct:pal("Change the fingerprint from CLI"),
	    lmaTestLib:changing_fingerprint_from_cli(?Fingerprint),
	    ct:pal("Install LKF"),
	    lmaTestLib:install_lkf(),
	    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
						?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),
	    {ok, {?LfciFeatureLicenseChangeInd, ?ClientRef, 0, ?LicenseState_Enabled, 
		  "CXC4020007"}} = rct_proxy:receive_proxy();
	nok ->
	    ct:pal("The fingerprint is already configured"),
	    ct:pal("Install LKF"),
	    lmaTestLib:install_lkf(),
	    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
						?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),
	    %% {ok, {?LfciFeatureLicenseChangeInd, ?ClientRef, _FS, _LS, 
	    %% 	  "CXC4020007"}} = rct_proxy:receive_proxy()
	    rct_proxy:receive_proxy()
    end,
    
    {FeatureStateMo, ok} = 
	lmaTestLib:find_mo_from_cli("FeatureState=CXC4020007", ?LM_MO, 
				    ["keyId=\"CXC4020007",
				     "description=\"Description of the feature"], 
				    ?NO_OF_TRIES, ok),    
    {_FeatureKeyMo, ok} = lmaTestLib:find_mo_from_cli("FeatureKey=CXC4020007", ?LM_MO, 
						      ["keyId=\"CXC4020007",
						       "name=\"Name of the feature"], 
						      ?NO_OF_TRIES, ok),
  
    %% Activate FeatureState and see that the license user is informed via LFCI
    ct:pal("Change the featureState for CXC4020007"),    
    State = lmaTestLib:change_feature_state(FeatureStateMo),

    FeatureState = case State of
		       "ACTIVATED" ->
			   ?ACTIVATED;
		       "DEACTIVATED" ->
			   ?DEACTIVATED
		   end,
    {ok, {?LfciFeatureLicenseChangeInd, ?ClientRef, FeatureState, _LicenseState, "CXC4020007"}} =  
	rct_proxy:receive_proxy(),

    %% test_server:break("Break"),


%%%%%%%%%%%%%%%%%%% Adding another client ref, ClientRef_2, and a new feature CXC1234567/1000

    ct:pal("Send LfciConnToServerReq to GLMS over LIHI with protocol version: ~p and client ref: ~p", 
	   [ProtocolVersion, ?ClientRef_2]),
    {ok,conn_to_server_req} = rct_proxy:send_proxy(node1, lihi1, ?LfciConnToServerReq, 
						   {ProtocolVersion, ?ClientRef_2}),
    ct:pal("Wait for LfciConnToServerCfm from GLMS"),
    {ok, {?LfciConnToServerCfm, ProtocolVersion, ServerRef_2}} = rct_proxy:receive_proxy(),

    ct:pal("Send LfciFeatureLicenseSubscribeReq for feature CXC1234567/1000 to GLMS over LIHI"),
    {ok, feature_license_subscribe_req} = 
	rct_proxy:send_proxy(node1, lihi1, ?LfciFeatureLicenseSubscribeReq, 
			     {ServerRef_2, ?ClientRef_2,"CXC1234567/1000", "test", 
			      "Madeup license description"}),
    {ok, {?LfciFeatureLicenseSubscribeCfm, ?ClientRef_2}} = rct_proxy:receive_proxy(),

    ct:pal("Check that the MO featureState has been created for CXC1234567/1000"),
    ok = lmaTestLib:check_data_from_cli(["FeatureState=CXC1234567/1000"], ?LM_MO, ?NO_OF_TRIES),

    ct:pal("Check that the featureState MO has correct description and keyId"),
    ok = lmaTestLib:check_data_from_cli(["description=\"Madeup license description", 
					 "keyId=\"CXC1234567/1000"], ?FEATURE_STATE_CXC1234567, 
					?NO_OF_TRIES),

    ct:pal("Make sure that the featureState for CXC1234567/1000 is ACTVIATED"),
    case lmaTestLib:check_data_from_cli(["featureState=ACTIVATED"], ?FEATURE_STATE_CXC1234567, 0) of
	ok ->
	    do_nada;
	nok ->
	    lmaTestLib:change_feature_state(?FEATURE_STATE_CXC1234567, "ACTIVATED"),
	    {ok, {?LfciFeatureLicenseChangeInd, ?ClientRef_2, ?ACTIVATED, ?LicenseState_Disabled, 
		  "CXC1234567/1000"}} = rct_proxy:receive_proxy()
    end,


    %% test_server:break("A"),

    %% Can only work if EU is not activated
    ct:pal("Check that the License Key Not Available alarm is visible"),
    {_, ok} = lmaTestLib:find_mo_from_cli("FmAlarm=", ?FM_MO, ["minorType=9175047"], 
					  ?NO_OF_TRIES, ok),
    
    ct:pal("Deactivate the feature and make sure that the License Key Not Available alarm is ceased"),
    lmaTestLib:change_feature_state(?FEATURE_STATE_CXC1234567, "DEACTIVATED"),
    {ok, {?LfciFeatureLicenseChangeInd, ?ClientRef_2, ?DEACTIVATED, ?LicenseState_Disabled, 
	  "CXC1234567/1000"}} = rct_proxy:receive_proxy(),
    
    {_, nok} =lmaTestLib:find_mo_from_cli("FmAlarm=", ?FM_MO, ["minorType=9175047"], 
    					  ?NO_OF_TRIES, nok),
    
    ct:pal("Send LfciFeatureLicenseUnsubscribeReq for CXC1234567 to GLMS over LIHI"),
    {ok, feature_license_unsubscribe_req} = 
    	rct_proxy:send_proxy(node1, lihi1, ?LfciFeatureLicenseUnsubscribeReq, 
    			     {ServerRef_2, ?ClientRef_2, "CXC1234567/1000"}),
    ct:pal("Wait for LfciFeatureLicenseUnsubscribeCfm from GLMS"),
    {ok, {?LfciFeatureLicenseUnsubscribeCfm, ?ClientRef_2}} = rct_proxy:receive_proxy(),

%%%%%%%%%%%%%%%%%%%

    ct:pal("Do another subscribe and make sure that the description is updated"),
    {ok, feature_license_subscribe_req} = 
	rct_proxy:send_proxy(node1, lihi1, ?LfciFeatureLicenseSubscribeReq, 
			     {ServerRef_2, ?ClientRef_2,"CXC1234567/1000", "test", 
			      "Another madeup license description"}),
    {ok, {?LfciFeatureLicenseSubscribeCfm, ?ClientRef_2}} = rct_proxy:receive_proxy(),

    ct:pal("Check that the featureState MO has correct description and keyId"),
    ok = lmaTestLib:check_data_from_cli(["description=\"Another madeup license description", 
					 "keyId=\"CXC1234567/1000"], ?FEATURE_STATE_CXC1234567, 
					?NO_OF_TRIES),
    
%%%%%%%%%%%%%%%%%%%

    ct:pal("Send LfciFeatureLicenseUnsubscribeReq for CXC4020007 to GLMS over LIHI"),
    {ok, feature_license_unsubscribe_req} = 
    	rct_proxy:send_proxy(node1, lihi1, ?LfciFeatureLicenseUnsubscribeReq, 
			     {ServerRef, ?ClientRef, "CXC4020007"}),
    ct:pal("Wait for LfciFeatureLicenseUnsubscribeCfm for CXC4020007 from GLMS"),
    {ok, {?LfciFeatureLicenseUnsubscribeCfm, ?ClientRef}} = rct_proxy:receive_proxy(),
    
    ct:pal("Send LfciFeatureLicenseUnsubscribeReq for a unknown CXC, CXC4011999, to GLMS over LIHI"),
    {ok, feature_license_unsubscribe_req} = 
    	rct_proxy:send_proxy(node1, lihi1, ?LfciFeatureLicenseUnsubscribeReq, 
    			     {ServerRef, ?ClientRef, "CXC4011999"}),
    ct:pal("Wait for LfciFeatureLicenseDisconnectInd for unknown CXC, CXC4011999, from GLMS"),
    {ok, {?LfciFeatureLicenseDisconnectInd, ?ClientRef}} = rct_proxy:receive_proxy(),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Testcases for the license feature control interface of vers 2 of LIHI.<br/>
%% @spec license_feature_control_interface_v2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
license_feature_control_interface_v2(_Config) ->
    ok = rct_cli:connect(cli),

    ProtocolVersion = 2,
    ct:pal("Send LfciConnToServerReq to GLMS over LIHI with protocol version: ~p", 
	   [ProtocolVersion]),
    {ok,conn_to_server_req} = rct_proxy:send_proxy(node1, lihi1, ?LfciConnToServerReq, 
						   {ProtocolVersion, ?ClientRef}),

    ct:pal("Wait for LfciConnToServerCfm from GLMS"),
    {ok, {?LfciConnToServerCfm, ProtocolVersion, ServerRef}} = rct_proxy:receive_proxy(),
    ct:pal("Connection to GLMS established over LIHI"),

    %% The license user subscribes for license controlled features
    ct:pal("Send LfciFeatureLicenseSubscribeReq to GLMS over LIHI"),
    {ok, feature_license_subscribe_req} = 
	rct_proxy:send_proxy(node1, lihi1, ?LfciFeatureLicenseSubscribeReq, 
			     {ServerRef, ?ClientRef,"CXC4011700", "Name of the feature", 
			      "Description of the feature"}),
    
    ct:pal("Wait for LfciFeatureLicenseSubscribeCfm from GLMS"),
    {ok, {?LfciFeatureLicenseSubscribe2Cfm, ?ClientRef, AlarmCorrelationEventId}} = rct_proxy:receive_proxy(),

    ct:pal("Got AlarmCorrelationEventId: ~p", [AlarmCorrelationEventId]),
    
    case lmaTestLib:check_if_fingerprint_updateable() of
 	ok ->
	    ct:pal("Check that the 'KeyFileFault' alarm is visible"),
	    {_Mo, ok} = lmaTestLib:find_mo_from_cli("FmAlarm=", ?FM_MO, 
						    ["minorType=9175046","activeSeverity=CRITICAL",
						     "value=\""++ 
							 integer_to_list(AlarmCorrelationEventId)], 
						    ?NO_OF_TRIES, ok),
	    ct:pal("Change the fingerprint from CLI"),
	    lmaTestLib:changing_fingerprint_from_cli(?Fingerprint),
	    ct:pal("Install LKF"),
	    lmaTestLib:install_lkf(),
	    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
						?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),
	    {ok, {?LfciFeatureLicenseChange2Ind, ?ClientRef, 0, ?LicenseState_Enabled, 
		  "CXC4011700", ?AlarmCorrelationCeased}} = rct_proxy:receive_proxy();
	nok ->
	    ct:pal("The fingerprint is already configured"),
	    ct:pal("Install LKF"),
	    lmaTestLib:install_lkf(),
	    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
						?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),
	    rct_proxy:receive_proxy()
    end,
    
    {FeatureStateMo, ok} = 
	lmaTestLib:find_mo_from_cli("FeatureState=CXC4011700", ?LM_MO, 
				    ["keyId=\"CXC4011700",
				     "description=\"Description of the feature"], 
				    ?NO_OF_TRIES, ok),    
    {_FeatureKeyMo, ok} = lmaTestLib:find_mo_from_cli("FeatureKey=CXC4011700", ?LM_MO, 
						      ["keyId=\"CXC4011700",
						       "name=\"Name of the feature"], 
						      ?NO_OF_TRIES, ok),
  
    %% Activate FeatureState and see that the license user is informed via LFCI
    ct:pal("Change the featureState for CXC4011700"),    
    State = lmaTestLib:change_feature_state(FeatureStateMo),

    FeatureState = case State of
		       "ACTIVATED" ->
			   ?ACTIVATED;
		       "DEACTIVATED" ->
			   ?DEACTIVATED
		   end,
    {ok, {?LfciFeatureLicenseChange2Ind, ?ClientRef, FeatureState, _LicenseState, "CXC4011700", 
	  ?AlarmCorrelationCeased}} = rct_proxy:receive_proxy(),

%%%%%%%%%%%%%%%%%%% Adding another client ref, ClientRef_2, and a new feature CXC1234567/2000

    ct:pal("Send LfciConnToServerReq to GLMS over LIHI with protocol version: ~p and client "
	   "ref: ~p", [ProtocolVersion, ?ClientRef_2]),
    {ok,conn_to_server_req} = rct_proxy:send_proxy(node1, lihi1, ?LfciConnToServerReq, 
						   {ProtocolVersion, ?ClientRef_2}),
    ct:pal("Wait for LfciConnToServerCfm from GLMS"),
    {ok, {?LfciConnToServerCfm, ProtocolVersion, ServerRef_2}} = rct_proxy:receive_proxy(),

    ct:pal("Send LfciFeatureLicenseSubscribeReq for feature CXC1234567/2000 to GLMS over LIHI"),
    {ok, feature_license_subscribe_req} = 
	rct_proxy:send_proxy(node1, lihi1, ?LfciFeatureLicenseSubscribeReq, 
			     {ServerRef_2, ?ClientRef_2,"CXC1234567/2000", "test", 
			      "Madeup license description"}),

    {ok, {?LfciFeatureLicenseSubscribe2Cfm, ?ClientRef_2, ?AlarmCorrelationCeased}} 
	= rct_proxy:receive_proxy(),

    ct:pal("Check that the MO featureState has been created for CXC1234567/2000"),
    ok = lmaTestLib:check_data_from_cli(["FeatureState=CXC1234567/2000"], ?LM_MO, 2),

    ct:pal("Check that the featureState MO has correct description and keyId"),
    ok = lmaTestLib:check_data_from_cli(["description=\"Madeup license description", 
					 "keyId=\"CXC1234567/2000"], ?FEATURE_STATE_CXC1234567_v2, 
					?NO_OF_TRIES),

    ct:pal("Make sure that the featureState for CXC1234567/2000 is ACTVIATED"),
    case lmaTestLib:check_data_from_cli(["featureState=ACTIVATED"], 
					?FEATURE_STATE_CXC1234567_v2, 0) of
	ok ->
	    %% Can only work if EU is not activated
	    ct:pal("Check that the License Key Not Available alarm is visible"),
	    {_, ok} = lmaTestLib:find_mo_from_cli("FmAlarm=", ?FM_MO, ["minorType=9175047"], 
						  ?NO_OF_TRIES, ok);
	nok ->
	    lmaTestLib:change_feature_state(?FEATURE_STATE_CXC1234567_v2, "ACTIVATED"),
	    {ok, {?LfciFeatureLicenseChange2Ind, ?ClientRef_2, ?ACTIVATED, ?LicenseState_Disabled, 
		  "CXC1234567/2000", AlarmCorrelationEventId2}} = rct_proxy:receive_proxy(),
	    ct:pal("Got AlarmCorrelationEventId2: ~p", [AlarmCorrelationEventId2]),
	    %% Can only work if EU is not activated
	    ct:pal("Check that the License Key Not Available alarm is visible"),
	    {_, ok} = lmaTestLib:find_mo_from_cli("FmAlarm=", ?FM_MO, 
						  ["minorType=9175047", 
						   "value=\""++ 
						       integer_to_list(AlarmCorrelationEventId2)], 
						  ?NO_OF_TRIES, ok)
    end,

    %% %% Can only work if EU is not activated
    %% ct:pal("Check that the License Key Not Available alarm is visible"),
    %% {_, ok} = lmaTestLib:find_mo_from_cli("FmAlarm=", ?FM_MO, ["minorType=9175047"], 
    %% 					  ?NO_OF_TRIES, ok),
    
    ct:pal("Deactivate the feature and make sure that the License Key Not Available alarm is ceased"),
    lmaTestLib:change_feature_state(?FEATURE_STATE_CXC1234567_v2, "DEACTIVATED"),
    {ok, {?LfciFeatureLicenseChange2Ind, ?ClientRef_2, ?DEACTIVATED, ?LicenseState_Disabled, 
	  "CXC1234567/2000", ?AlarmCorrelationCeased}} = rct_proxy:receive_proxy(),
    
    {_, nok} =lmaTestLib:find_mo_from_cli("FmAlarm=", ?FM_MO, ["minorType=9175047"], 
    					  ?NO_OF_TRIES, nok),
    
    ct:pal("Send LfciFeatureLicenseUnsubscribeReq for CXC1234567 to GLMS over LIHI"),
    {ok, feature_license_unsubscribe_req} = 
    	rct_proxy:send_proxy(node1, lihi1, ?LfciFeatureLicenseUnsubscribeReq, 
    			     {ServerRef_2, ?ClientRef_2, "CXC1234567/2000"}),
    ct:pal("Wait for LfciFeatureLicenseUnsubscribeCfm from GLMS"),
    {ok, {?LfciFeatureLicenseUnsubscribeCfm, ?ClientRef_2}} = rct_proxy:receive_proxy(),
    
    
%%%%%%%%%%%%%%%%%%%

    ct:pal("Send LfciFeatureLicenseUnsubscribeReq for CXC4011700 to GLMS over LIHI"),
    {ok, feature_license_unsubscribe_req} = 
    	rct_proxy:send_proxy(node1, lihi1, ?LfciFeatureLicenseUnsubscribeReq, 
			     {ServerRef, ?ClientRef, "CXC4011700"}),
    ct:pal("Wait for LfciFeatureLicenseUnsubscribeCfm for CXC4011700 from GLMS"),
    {ok, {?LfciFeatureLicenseUnsubscribeCfm, ?ClientRef}} = rct_proxy:receive_proxy(),
    
    ct:pal("Send LfciFeatureLicenseUnsubscribeReq for a unknown CXC, CXC4011999, to GLMS over LIHI"),
    {ok, feature_license_unsubscribe_req} = 
    	rct_proxy:send_proxy(node1, lihi1, ?LfciFeatureLicenseUnsubscribeReq, 
    			     {ServerRef, ?ClientRef, "CXC4011999"}),
    ct:pal("Wait for LfciFeatureLicenseDisconnectInd for unknown CXC, CXC4011999, from GLMS"),
    {ok, {?LfciFeatureLicenseDisconnectInd, ?ClientRef}} = rct_proxy:receive_proxy(),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Testcases for the license capacity control interface of LIHI.<br/>
%% @spec license_capacity_control_interface(Config) -> ok
%% @end
%%--------------------------------------------------------------------
license_capacity_control_interface(_Config) ->
    ok = rct_cli:connect(cli),

%%%%%%%%%%%%%%%%%%% ConnToServerReq with incorrect protocol version

    IncorrectProtocolVestion = 3,
    ct:pal("Send LcciConnToServerReq to GLMS over LIHI with incorrect protocol version: ~p", 
    	   [IncorrectProtocolVestion]),
     {ok,conn_to_server_req} = rct_proxy:send_proxy(node1, lihi1, ?LcciConnToServerReq, 
						    {IncorrectProtocolVestion}),
    ct:pal("Wait for LcciConnToServerRej from GLMS"),
    {ok, {?LcciConnToServerRej}} = rct_proxy:receive_proxy(),

%%%%%%%%%%%%%%%%%%% ConnToServerReq

    ProtocolVersion = 1,
    ct:pal("Send LcciConnToServerReq to GLMS over LIHI with protocol version: ~p", 
	   [ProtocolVersion]),
    {ok,conn_to_server_req} = rct_proxy:send_proxy(node1, lihi1, ?LcciConnToServerReq, 
						   {ProtocolVersion, ?ClientRef}),
    ct:pal("Wait for LcciConnToServerCfm from GLMS"),
    {ok, {?LcciConnToServerCfm, ProtocolVersion, ServerRef}} = rct_proxy:receive_proxy(),
    ct:pal("Connection to GLMS established over LIHI"),
    ct:pal("ServerRef from GLMS in connToServerCfm: ~p", [ServerRef]),

   case lmaTestLib:check_if_fingerprint_updateable() of
 	ok ->

           %% %%%%%%%%%%%%%%%%% LicenseSubscribeReq

	   ct:pal("Send LcciCapacityLicenseSubscribeReq to GLMS over LIHI"),
	   {ok, capacity_license_subscribe_req} = 
	       rct_proxy:send_proxy(node1, lihi1, ?LcciCapacityLicenseSubscribeReq, 
	   			    {ServerRef, ?ClientRef,"CXC4010719", "Name of the capacity", 
	   			     "Description of the capacity", "No of cups of coffee", 
				     ?GP_AVAILABLE_1, ?SW_CAPACITYTYPE}),
	   %% Receive licenseState = DISABLED  & value = 0 since no LKF is installed
	   ct:pal("Wait for LccipacityLicenseSubscribeCfm from GLMS"),
	   {ok, {?LcciCapacityLicenseSubscribeCfm, 1, ?ClientRef, ?LicenseState_Disabled, 
		 ?GP_monitoring_shall_be_done_1, ?ActivationThreshold_default, ?CapacityNoValue_0, 
		 ?NoLimit_deactive_0}} = rct_proxy:receive_proxy(),
	   
	    ct:pal("Change the fingerprint from CLI"),
	    lmaTestLib:changing_fingerprint_from_cli(?Fingerprint),
	   	 
	   ct:pal("Install LKF"),
	   JenkinsNode=lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
	   TEST_DIR = lmaTestLib:get_test_dir(),
	   
	   {ok,_} = file:copy(TEST_DIR++"RCS_MSR_141124_094406.xml", ?PROJ_DIR++JenkinsNode
	   		      ++"/RCS_MSR_141124_094406.xml"),
	   lmaTestLib:install_lkf(?PROJ_DIR, "RCS_MSR_141124_094406.xml"),

	   ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
						?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),

           %% %%%%%%%%%%%%%%%%% LcciCapacityLicenseChangeInd
	   
	   case rct_proxy:receive_proxy() of
	       {ok, {?LcciCapacityLicenseChangeInd, ?ClientRef, ?LicenseState_Enabled, 
		     ?GP_monitoring_shall_be_done_1, ?ActivationThreshold_default, 18, 
		     ?NoLimit_deactive_0}} ->
		   ct:pal("Wait for another changeInd over LIHI"),
		   {ok, {?LcciCapacityLicenseChangeInd, ?ClientRef, ?LicenseState_Enabled, 
			 ?GP_monitoring_shall_be_done_1, ?ActivationThreshold_default, 
			 ?CapacityValue_1, ?NoLimit_deactive_0}} = rct_proxy:receive_proxy();
	       {ok, {?LcciCapacityLicenseChangeInd, ?ClientRef, ?LicenseState_Enabled, 
		     ?GP_monitoring_shall_be_done_1, ?ActivationThreshold_default, 
		     ?CapacityValue_1, ?NoLimit_deactive_0}} ->
		   ok
	   end;
	
	   %% %%%%%%%%%%%%%%%%%%% Update grace period atrributes
	   %% TODO, sort this out. Not correct values relcieved from LIHI?
	   %% ct:pal("Update grace period atrributes for CXC4010719"),
	   %% ok = rct_rpc:call(rpc_1, lmaGlms, update_gp_attributes, ["CXC4010719", 2, 20, 30], 1000),
	   
	   %% {ok, {?LcciCapacityLicenseChangeInd, ?ClientRef, ?LicenseState_Enabled, 
	   %% 	 ?GP_monitoring_shall_be_done_1, ActivationThreshold, ?CapacityValue_1, 
	   %% 	 ?NoLimit_deactive_0}} = rct_proxy:receive_proxy(),
	   %% ct:pal("ActivationThreshold at update GP attr: ~p", [ActivationThreshold]),
	   %% test_server:break("A");

       nok ->
	   ct:pal("The fingerprint is already updated"),

	   ct:pal("Install LKF"),
	   JenkinsNode=lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),

	   TEST_DIR = lmaTestLib:get_test_dir(),
	   
	   {ok,_} = file:copy(TEST_DIR++"RCS_MSR_141124_094406.xml", ?PROJ_DIR++JenkinsNode
	   		      ++"/RCS_MSR_141124_094406.xml"),
	   lmaTestLib:install_lkf(?PROJ_DIR, "RCS_MSR_141124_094406.xml"),

	   ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
						?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),
	   
           %% %%%%%%%%%%%%%%%%% LicenseSubscribeReq

	   ct:pal("Send LcciCapacityLicenseSubscribeReq to GLMS over LIHI"),
	   {ok, capacity_license_subscribe_req} = 
	       rct_proxy:send_proxy(node1, lihi1, ?LcciCapacityLicenseSubscribeReq, 
				    {ServerRef, ?ClientRef,"CXC4010719", "Name of the capacity", 
				     "Description of the capacity", "No of cups of coffee", 
				     ?GP_AVAILABLE_1, ?SW_CAPACITYTYPE}),
    
	   ct:pal("Wait for LcciCapacityLicenseSubscribeCfm from GLMS, check if the GP alarm "
		  "is visible"),
	   case lmaTestLib:find_mo_from_cli("FmAlarm=", ?FM_MO, 
					    ["minorType=9175051", 
					     "source=\"ManagedElement=1,SystemFunctions=1,Lm=1,"
					     "CapacityState=CXC4010719,GracePeriod=CXC4010719"], 
					    1, ok) of
	       {_, ok} ->
		   {ok, {?LcciCapacityLicenseSubscribeCfm, _SizeOfLicenseInfo, ?ClientRef, 
			 ?LicenseState_Enabled, 
			 ?GP_monitoring_shall_NOT_be_done_0, _ActivationThreshold, 
			 ?CapacityValue_1, ?NoLimit_active_1}} = rct_proxy:receive_proxy();
	       {_, nok} ->
		   {ok, {?LcciCapacityLicenseSubscribeCfm, _SizeOfLicenseInfo, ?ClientRef, 
			 ?LicenseState_Enabled, 
			 ?GP_monitoring_shall_be_done_1, ?ActivationThreshold_default, 
			 ?CapacityValue_1, ?NoLimit_deactive_0}} = rct_proxy:receive_proxy()
	   end
   end,

    {_, ok} = 
	lmaTestLib:find_mo_from_cli("CapacityState=CXC4010719", ?LM_MO, 
				    ["keyId=\"CXC4010719",
				     "description=\"Description of the capacity"], 
				    ?NO_OF_TRIES, ok),
    
    ct:pal("Check that the GP MO is created for CXC4010719"),
    {_, ok} = lmaTestLib:find_mo_from_cli("GracePeriod=CXC4010719", ?LM_MO 
					  ++",CapacityState=CXC4010719", 
    					  ["gracePeriodId=\"CXC4010719"], 
    					  ?NO_OF_TRIES, ok),

      %%%%%%%%%%%%%%%%%%% Adding another client ref, ClientRef_2, and a new capacity CXC4011999

     %%%%%%%%%%%%%%%%%%% LicenseSubscribeReq

    ct:pal("Send LcciCapacityLicenseSubscribeReq for a unknown CXC, CXC4011999, to GLMS over LIHI"),
    {ok, capacity_license_subscribe_req} = 
    	rct_proxy:send_proxy(node1, lihi1, ?LcciCapacityLicenseSubscribeReq, 
			     {ServerRef, ?ClientRef_2, "CXC4011999", "Fake name of the capacity", 
			      "Fake description of the capacity", "No of cups of tea", 
			      ?GP_NOT_AVAILABLE_0, ?SW_CAPACITYTYPE}),
    ct:pal("Wait for LcciCapacityLicenseSubscribeCfm for unknown CXC, CXC4011999, from GLMS"),
    {ok, {?LcciCapacityLicenseSubscribeCfm, _Size, ?ClientRef_2, ?LicenseState_Disabled, 
	  ?GP_monitoring_shall_NOT_be_done_0, ?ActivationThreshold_default, ?CapacityNoValue_0, 
	  ?NoLimit_deactive_0}} = rct_proxy:receive_proxy(),

    ct:pal("Send LcciCapacityLicenseSubscribeReq for yet another unknown CXC, CXC4011777, to GLMS over LIHI"),
    {ok, capacity_license_subscribe_req} = 
    	rct_proxy:send_proxy(node1, lihi1, ?LcciCapacityLicenseSubscribeReq, 
			     {ServerRef, ?ClientRef_2, "CXC4011777", "Bla", 
			      "Bla", "Bla", ?GP_AVAILABLE_1, ?SW_CAPACITYTYPE}),
    ct:pal("Wait for LcciCapacityLicenseSubscribeCfm for unknown CXC, CXC4011777, from GLMS"),
    {ok, {?LcciCapacityLicenseSubscribeCfm, _Size, ?ClientRef_2, ?LicenseState_Disabled, 
	  ?GP_monitoring_shall_be_done_1, ?ActivationThreshold_default, ?CapacityNoValue_0, 
	  ?NoLimit_deactive_0}} = rct_proxy:receive_proxy(),
    
    {_, ok} = lmaTestLib:find_mo_from_cli("CapacityState=CXC4011999", ?LM_MO, 
    					  ["keyId=\"CXC4011999",
    					   "description=\"Fake description of the capacity"], 
    					  ?NO_OF_TRIES, ok), 
    ct:pal("Checka that the GP MO is NOT created for CXC4011999"),
    {_, nok} = lmaTestLib:find_mo_from_cli("GracePeriod=CXC4011999", ?LM_MO
					   ++",CapacityState=CXC4011999", 
					   ["gracePeriodId=\"CXC4011999"], 1, nok),

    {_, ok} = lmaTestLib:find_mo_from_cli("CapacityState=CXC4011777", ?LM_MO, 
    					  ["keyId=\"CXC4011777",
    					   "description=\"Bla"], 
    					  ?NO_OF_TRIES, ok), 
    ct:pal("Checka that the GP MO is created for CXC4011777"),
    {_, ok} = lmaTestLib:find_mo_from_cli("GracePeriod=CXC4011777", ?LM_MO
					  ++",CapacityState=CXC4011777", 
					  ["gracePeriodId=\"CXC4011777"], 1, ok),

    %% License Key Not Available alarm not supported for capacity keys
    ct:pal("Check that the License Key Not Available alarm is NOT visible"),
    {_, nok} = lmaTestLib:find_mo_from_cli("FmAlarm=", ?FM_MO, ["minorType=9175047"], 
					  ?NO_OF_TRIES, nok),

    %% Add another client for CXC4011999 that would like to have GP but the MO will not be created 
    %% since ClientRef_2 has a subscribtion with GP = Not available.
    {ok,conn_to_server_req} = rct_proxy:send_proxy(node1, lihi1, ?LcciConnToServerReq, 
						   {ProtocolVersion, ?ClientRef_3}),
    {ok, {?LcciConnToServerCfm, ProtocolVersion, ServerRef2}} = rct_proxy:receive_proxy(),
    ct:pal("ServerRef2 from GLMS in connToServerCfm: ~p", [ServerRef2]),
    ct:pal("Send LcciCapacityLicenseSubscribeReq for CXC4011999 but clientRef_3"),
    {ok, capacity_license_subscribe_req} = 
    	rct_proxy:send_proxy(node1, lihi1, ?LcciCapacityLicenseSubscribeReq, 
    			     {ServerRef2, ?ClientRef_3, "CXC4011999", "Another fake name of the "
			      "capacity", "Another fake description of the capacity", 
			      "No of jugs of tea", ?GP_AVAILABLE_1, ?SW_CAPACITYTYPE}),
    ct:pal("Wait for LcciCapacityLicenseSubscribeCfm for CXC4011999"),
    {ok, {?LcciCapacityLicenseSubscribeCfm, _Size, ?ClientRef_3, ?LicenseState_Disabled, 
	  ?GP_monitoring_shall_NOT_be_done_0, ?ActivationThreshold_default, ?CapacityNoValue_0, 
	  ?NoLimit_deactive_0}} = rct_proxy:receive_proxy(),
    
    {_, ok} = lmaTestLib:find_mo_from_cli("CapacityState=CXC4011999", ?LM_MO, 
    					  ["keyId=\"CXC4011999",
    					   "description=\"Fake description of the capacity"], 
    					  ?NO_OF_TRIES, ok), 
    ct:pal("Checka that the GP MO is NOT created for CXC4011999"),
    {_, nok} = lmaTestLib:find_mo_from_cli("GracePeriod=CXC4011999", ?LM_MO++
					       ",CapacityState=CXC4011999", 
    					   ["gracePeriodId=\"CXC4011999"], 1, nok),  

    
%%%%%%%%%%%%%%%%%%% UnsubscribeReq

    ct:pal("Send LcciCapacityLicenseUnsubscribeReq to GLMS over LIHI"),
    {ok, capacity_license_unsubscribe_req} = 
    	rct_proxy:send_proxy(node1, lihi1, ?LcciCapacityLicenseUnsubscribeReq, 
			     {ServerRef, ?ClientRef_2, "CXC4011999"}),
    ct:pal("Wait for LcciCapacityLicenseUnsubscribeCfm from GLMS"),
    {ok, {?LcciCapacityLicenseUnsubscribeCfm, ?ClientRef_2}} = rct_proxy:receive_proxy(),
    
    {ok, capacity_license_unsubscribe_req} = 
    	rct_proxy:send_proxy(node1, lihi1, ?LcciCapacityLicenseUnsubscribeReq, 
    			     {ServerRef2, ?ClientRef_3, "CXC4011999"}),
    ct:pal("Wait for LcciCapacityLicenseUnsubscribeCfm from GLMS"),
    {ok, {?LcciCapacityLicenseUnsubscribeCfm, ?ClientRef_3}} = rct_proxy:receive_proxy(),


    ct:pal("Send LcciCapacityLicenseUnsubscribeReq to GLMS over LIHI"),
    {ok, capacity_license_unsubscribe_req} = 
    	rct_proxy:send_proxy(node1, lihi1, ?LcciCapacityLicenseUnsubscribeReq, 
			     {ServerRef, ?ClientRef, "CXC4010719"}),
    ct:pal("Wait for LcciCapacityLicenseUnsubscribeCfm from GLMS"),
    {ok, {?LcciCapacityLicenseUnsubscribeCfm, ?ClientRef}} = rct_proxy:receive_proxy(),

    ct:pal("Check that the License Key Not Available alarm has ceased"),
    {_, nok} = lmaTestLib:find_mo_from_cli("FmAlarm=", ?FM_MO, ["minorType=9175047"], 
					   ?NO_OF_TRIES, nok),

    ct:pal("Send LcciCapacityLicenseUnsubscribeReq for a unknown CXC to GLMS over LIHI"),
    {ok, capacity_license_unsubscribe_req} = 
    	rct_proxy:send_proxy(node1, lihi1, ?LcciCapacityLicenseUnsubscribeReq, 
			     {ServerRef, ?ClientRef, "CXC4011888"}),
    ct:pal("Wait for LcciCapacityLicenseDisconnectInd for unknown CXC from GLMS"),
    {ok, {?LcciCapacityLicenseDisconnectInd, ?ClientRef}} = rct_proxy:receive_proxy(),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% Testcases for the license capacity control interface of LIHI.<br/>
%% @spec license_capacity_control_interface_v2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
license_capacity_control_interface_v2(_Config) ->
    ok = rct_cli:connect(cli),
    
    %% %%%%%%%%%%%%%%%%% ConnToServerReq
    
    %% Version 1
    ProtocolVersion1 = 1,
    {ok,conn_to_server_req} = rct_proxy:send_proxy(node1, lihi1, ?LcciConnToServerReq, 
						   {ProtocolVersion1, ?ClientRef_3}),
    {ok, {?LcciConnToServerCfm, ProtocolVersion1, ServerRef3}} = rct_proxy:receive_proxy(),
    
    %% Version 2
    ProtocolVersion = 2,
    ct:pal("Send LcciConnToServerReq to GLMS over LIHI with protocol version: ~p", 
	   [ProtocolVersion]),
    {ok,conn_to_server_req} = rct_proxy:send_proxy(node1, lihi1, ?LcciConnToServerReq, 
						   {ProtocolVersion, ?ClientRef}),
    ct:pal("Wait for LcciConnToServerCfm from GLMS"),
    {ok, {?LcciConnToServerCfm, ProtocolVersion, ServerRef}} = rct_proxy:receive_proxy(),
    
    case lmaTestLib:check_if_fingerprint_updateable() of
	ok ->
	    %% %%%%%%%%%%%%%%%%% LicenseSubscribeReq
	    
	    ct:pal("Send LcciCapacityLicenseSubscribeReq to GLMS over LIHI"),
	    {ok, capacity_license_subscribe_req} = 
		rct_proxy:send_proxy(node1, lihi1, ?LcciCapacityLicenseSubscribeReq, 
				     {ServerRef, ?ClientRef,"CXC4011961", 
				      "Name of the capacity", 
				      "Description of the capacity", 
				      "No of cups of coffee", 
				      ?GP_AVAILABLE_1, ?SW_CAPACITYTYPE}),
	    
	    ct:pal("Wait for LccipacityLicenseSubscribe2Cfm from GLMS"),
	    {ok, {?LcciCapacityLicenseSubscribe2Cfm, 1, ?ClientRef, ?LicenseState_Disabled, 
		  ?GP_monitoring_shall_be_done_1, ?ActivationThreshold_default, 
		  ?CapacityNoValue_0, ?NoLimit_deactive_0, []}} = rct_proxy:receive_proxy(),
	    
	    %% Set fingerprint and Install LKF
	    ct:pal("Change the fingerprint from CLI"),
	    lmaTestLib:changing_fingerprint_from_cli(?Fingerprint),
	    ct:pal("Install LKF"),
	    
	    lmaTestLib:install_lkf(),
	    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
						?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),
	    
	    ct:pal("Wait for LcciCapacityLicenseChange2Ind from GLMS"),
	    {ok, {?LcciCapacityLicenseChange2Ind, ?ClientRef, ?LicenseState_Enabled, 
		  ?GP_monitoring_shall_be_done_1, ?ActivationThreshold_default, 1, 
		  ?NoLimit_deactive_0, "L:;W:;G:"}} = rct_proxy:receive_proxy();
	
	nok ->
	    ct:pal("Install LKF"),
	    lmaTestLib:install_lkf(),
	    lmaTestLib:wait_for_install_complete(),
	    
	    ct:pal("Send LcciCapacityLicenseSubscribeReq to GLMS over LIHI"),
	    
	    %% Version 1
	    {ok, capacity_license_subscribe_req} = 
		rct_proxy:send_proxy(node1, lihi1, ?LcciCapacityLicenseSubscribeReq, 
				     {ServerRef3, ?ClientRef_3,"CXC4011961", 
				      "Name of the capacity", 
				      "Description of the capacity", 
				      "No of cups of coffee", 
				      ?GP_AVAILABLE_1, ?SW_CAPACITYTYPE}),
	    {ok, {?LcciCapacityLicenseSubscribeCfm, _S, ?ClientRef_3, 
		  ?LicenseState_Enabled, ?GP_monitoring_shall_be_done_1, 
		  ?ActivationThreshold_default, 1, ?NoLimit_deactive_0}} 
		= rct_proxy:receive_proxy(),
	    
	    %% Version 2
	    {ok, capacity_license_subscribe_req} = 
		rct_proxy:send_proxy(node1, lihi1, ?LcciCapacityLicenseSubscribeReq, 
				     {ServerRef, ?ClientRef,"CXC4011961", 
				      "Name of the capacity", 
				      "Description of the capacity", 
				      "No of cups of coffee", 
				      ?GP_AVAILABLE_1, ?SW_CAPACITYTYPE}),
	    
	    ct:pal("Wait for LccipacityLicenseSubscribe2Cfm from GLMS"),
	    {ok, {?LcciCapacityLicenseSubscribe2Cfm, 1, ?ClientRef, ?LicenseState_Enabled, 
		  ?GP_monitoring_shall_be_done_1, ?ActivationThreshold_default, 1, 
		  ?NoLimit_deactive_0, "L:;W:;G:"}} = rct_proxy:receive_proxy()
    end,
    
    {ok, capacity_license_unsubscribe_req} = 
	rct_proxy:send_proxy(node1, lihi1, ?LcciCapacityLicenseUnsubscribeReq, 
    			     {ServerRef, ?ClientRef, "CXC4011961"}),
    ct:pal("Wait for LcciCapacityLicenseUnsubscribeCfm from GLMS"),
    {ok, {?LcciCapacityLicenseUnsubscribeCfm, ?ClientRef}} = rct_proxy:receive_proxy(),
    
    {ok, capacity_license_unsubscribe_req} = 
	rct_proxy:send_proxy(node1, lihi1, ?LcciCapacityLicenseUnsubscribeReq, 
			     {ServerRef3, ?ClientRef_3, "CXC4011961"}),
    {ok, {_UnsubscribeCmfOrDisconnectInd, ?ClientRef_3}} = rct_proxy:receive_proxy(),
    ok.
   
%%%%%%%%%%%%%%%%%%% 
%%%%%%%%%%%%%%%%%%% 

    %% ct:pal("Restart node"),
    
    %% ok = rct_rpc:call(rpc_1, appmI, restart_piu_cold, ['_'], 100000, noprint),
    %% {ok,_} = ct_telnet:expect(console, "Restarting system",
    %% 			      [{timeout,30000}, no_prompt_check]), 
    %% ct:pal("Matched: Restarting system , recieved.", []),
    %% {ok, _} = ct_telnet:expect(console, "login:", [{timeout,60000}, no_prompt_check]),
    
    %% ct:pal("Restart node done"),

    %% wait_for_netconf_started(),
    %% {ok, client_started} = rct_proxy:start_proxy(node1, lihi1, ?LIHI),
    %% ok = rct_cli:connect(cli),

      
    %% ct:pal("Send LcciConnToServerReq to GLMS over LIHI with protocol version: ~p", 
    %% 	   [ProtocolVersion]),
    %% {ok,conn_to_server_req} = rct_proxy:send_proxy(node1, lihi1, ?LcciConnToServerReq, 
    %% 						   {ProtocolVersion, ?ClientRef}),
    
    %% ct:pal("Wait for LcciConnToServerCfm from GLMS"),
    %% {ok, {?LcciConnToServerCfm, ProtocolVersion, ServerRef}} = rct_proxy:receive_proxy(),

    %% ct:pal("Send LcciCapacityLicenseSubscribeReq to GLMS over LIHI"),
    %% {ok, capacity_license_subscribe_req} = 
    %% 	rct_proxy:send_proxy(node1, lihi1, ?LcciCapacityLicenseSubscribeReq, 
    %% 			     {ServerRef, ?ClientRef,"CXC4011822", "Name of the capacity", 
    %% 			      "Description of the capacity", "No of cups of coffee", ?GP_AVAILABLE_1, 
    %% 			      ?SW_CAPACITYTYPE}),
    
    %% ct:pal("Wait for LcciCapacityLicenseSubscribeCfm from GLMS"),
    %% {ok, {?LcciCapacityLicenseSubscribeCfm, ?ClientRef, ?LicenseState_Enabled, ?CapacityValue_1}} = rct_proxy:receive_proxy(),

%%%%%%%%%%%%%%%%%%% 
%%%%%%%%%%%%%%%%%%% 

    
%%% Internal functions


%% wait_for_netconf_started() ->
%%     wait_for_netconf_started(150000).

%% wait_for_netconf_started(Timeout) when Timeout < 500 ->
%%     ct:fail("Netconf not started within max timeout after restart.");
%% wait_for_netconf_started(Timeout) ->
%%     case ct_netconfc:open(nc1,[]) of
%%     	{ok,_} ->
%% 	    ct:pal("netconf open - ok.",[]);
%% 	{error,{ssh,could_not_connect_to_server,econnrefused}} ->
%% 	    timer:sleep(250),
%% 	    wait_for_netconf_started(Timeout - 250);
%% 	_Other ->
%% 	    timer:sleep(250),
%% 	    wait_for_netconf_started(Timeout - 250)
%%     end.



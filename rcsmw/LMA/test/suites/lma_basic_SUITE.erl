%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lma_basic_SUITE.erl %
%%% @author etxpejn
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R3A/R4A/R5A/R9A/1
%%%
%%% @doc Test suite for testing the license feature control interface of LIHI.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(lma_basic_SUITE).
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
%%% R3A/1      2015-01-21  etxpejn     Create basic tests without PKI check on LKFs.
%%% R4A/6      2015-06-17  etxpejn     Added tc load_test_mom
%%% R5A/3      2016-02-12  etxkols     Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
%%% R5A/5      2016-06-21  etxkols     GIT migration
%%% R9A/1      2017-03-30  etxpejn     Changes due to new LKF
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
	 autonomous_mode/1,
	 grace_period/1,
	 change_capacity_value/1,
	 check_featureState/1,
	 failsafe_restart/1,
	 license_capacity_with_ncl/1,
	 load_test_mom/1,
	 capacity_expires/1,
	 embargo_ipsec/1
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
		 {rct_proxy,[{1, node1, ssh_lmt_ipv4, du1, username}]},
		 {rct_core,[]},
		 {rct_rs232,console},
		 {rct_netconf, nc1}
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
    Timer = 21600, % 6 hours turns into 1 sec
    ct:pal("Restart GLMS with adjusted timer so that 6 hours turns into 1 sec"),
    rct_rpc:call(rpc_1, lmaGlms, adjust_glms_timer_test, [Timer], 10000),
    timer:sleep(2000),
    {ok, client_started} = rct_proxy:start_proxy(node1, lihi1, ?LIHI),
    Config.

%% @hidden
%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = [tuple()]
%% Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    {ok, _} = rct_proxy:stop_proxy(node1, lihi1),
    ok = rct_proxy:exit_master(node1),
    ct:pal("Restart GLMS to get default adjusted timer"),
    rct_rpc:call(rpc_1, lmaGlms, adjust_glms_timer, [["-d"]], 10000),
    timer:sleep(2000),
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
    ok = rct_rpc:call(rpc_1, lmaGlms, change_test_mode, [off], 100000, noprint),
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
    [{group, basic_test}].

%%--------------------------------------------------------------------
%% @doc
%% TCs runed by jenkins.
%% @end
%%--------------------------------------------------------------------
groups() ->
    [{basic_test, [sequence],
      [
       autonomous_mode,
       grace_period,
       change_capacity_value,
       license_capacity_with_ncl,
       check_featureState,
       %% load_test_mom,
       embargo_ipsec
      ]}].

%%--------------------------------------------------------------------
%% @doc
%% Testcases that activated autonomous mode and  and checks that the MO and alarm
%% are updated correctly.<br/>
%% <pre> 
%% slogan= Autonomous Mode activated 
%% useCase= NodeUC453.N1
%% requirementId=
%% non-requirement=
%% IWD= 
%% addTR=
%% </pre>
%% <pre> 
%% slogan= Autonomous Mode deactivated 
%% useCase= NodeUC453.A1
%% requirementId=
%% non-requirement=
%% IWD= 
%% addTR=
%% </pre>
%% <pre> 
%% slogan= Autonomous Mode expires
%% useCase= NodeUC453.A2
%% requirementId=
%% non-requirement=
%% IWD= 
%% addTR=
%% </pre>
%% @end
%% @spec autonomous_mode(Config) -> ok
%% @end
%%--------------------------------------------------------------------
autonomous_mode(_Config) ->
    ok = rct_cli:connect(cli),
    ok = rct_rpc:call(rpc_1, lmaGlms, change_test_mode, [on], 100000, noprint),
    case lmaTestLib:check_if_fingerprint_updateable() of
 	ok ->
	    ct:pal("Change the fingerprint from CLI"),
	    lmaTestLib:changing_fingerprint_from_cli(?Fingerprint);
	nok ->
	    ct:pal("Fingerprint is already set")
    end,

    ct:pal("Install LKF"),
    lmaTestLib:install_lkf(),
    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
					?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),

    ct:pal("Change the featureState to ACTIVATED for FeatureState=CXC4011921"),
    lmaTestLib:change_feature_state("ManagedElement=1,SystemFunctions=1,"
				    "Lm=1,FeatureState=CXC4011921", "ACTIVATED"),
    
    ct:pal("Check from CLI that the featureState has changed for CXC4011921"),
    ok = lmaTestLib:check_data_from_cli(["featureState=ACTIVATED", "licenseState=ENABLED", 
					 "serviceState=OPERABLE"], 
					"ManagedElement=1,SystemFunctions=1,"
					"Lm=1,FeatureState=CXC4011921", ?NO_OF_TRIES),

    {ServerRef, ?LicenseState_Enabled, ?GP_monitoring_shall_be_done_1, _, 1000, 
     ?NoLimit_deactive_0} 
	= lmaTestLib:connect_and_subscribe_capcity_over_lihi("CXC4010624", ?GP_AVAILABLE_1),
    
    ct:pal("Install LKF that will trigger AM"),
    JenkinsNode=lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
    TEST_DIR = lmaTestLib:get_test_dir(),
    {ok,_} = file:copy(TEST_DIR++"test.xml", ?PROJ_DIR++JenkinsNode++"/test.xml"),
    lmaTestLib:install_lkf(?PROJ_DIR, "test.xml"),

    %% test_server:break("Break"),


    ct:pal("Check if Autonomous Mode is ACTIVATED_EXPIRING"),
    {_, ok} = lmaTestLib:find_mo_from_cli_init("AutonomousMode=1", ?LM_MO,
					       ["activationState=ACTIVATED_EXPIRING"], 
					       ?NO_OF_TRIES, ok),
    ct:pal("Check if Autonomous Mode alarm is visible"),
    {_, ok} = lmaTestLib:find_mo_from_cli_init("FmAlarm=", ?FM_MO, 
					       ["minorType=9175049","source=\"ManagedElement=1,"
						"SystemFunctions=1,Lm=1,AutonomousMode=1",
						"activeSeverity=MAJOR"], 
					       ?NO_OF_TRIES, ok),
    ct:pal("The alarm should be ceased after 36 hours"),
    {_, nok} = lmaTestLib:find_mo_from_cli_init("FmAlarm=", ?FM_MO, 
						["minorType=9175049","source=\"ManagedElement=1,"
						 "SystemFunctions=1,Lm=1,AutonomousMode=1"], 
						?NO_OF_TRIES, nok),
    ct:pal("Check if Autonomous Mode is INACTIVE"),
    {_, ok} = lmaTestLib:find_mo_from_cli_init("AutonomousMode=1", ?LM_MO,
					       ["activationState=INACTIVE"], ?NO_OF_TRIES, ok),
    ct:pal("Receive LcciCapacityLicenseChangeInd"),
    {ok, {?LcciCapacityLicenseChangeInd, ?ClientRef, ?LicenseState_Disabled, 
	  ?GP_monitoring_shall_be_done_1, _ActivationThreshold, _, ?NoLimit_deactive_0}} = 
	rct_proxy:receive_proxy(),

    ct:pal("Check from CLI that the featureState has changed to DISABLED for CXC4011921"),
    ok = lmaTestLib:check_data_from_cli(["featureState=ACTIVATED", "licenseState=DISABLED", 
					 "serviceState=INOPERABLE"], 
					"ManagedElement=1,SystemFunctions=1,"
					"Lm=1,FeatureState=CXC4011921", ?NO_OF_TRIES),
    
    ct:pal("Test case completeted OK!"),
        
    ok = lmaTestLib:unsubscribe_capcity_over_lihi("CXC4010719", ServerRef),

    ct:pal("Cleanup - Install correct LKF"),
    lmaTestLib:install_lkf(),
    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
					?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Testcases check that the GP is reset, both the MO and the alarm, after installing 
%% a new LKF.<br/>
%% NodeUC452.N1: Grace Period activated
%% NodeUC452.A1: Grace Period is about to expire
%% NodeUC452.A2: Grace Period expires
%% NodeUC452.A3: Grace Period is consumed
%% NodeUC448.A2: GP is reset
%% @spec grace_period(Config) -> ok
%% @end
%%--------------------------------------------------------------------
grace_period(_Config) ->
    ok = rct_cli:connect(cli),
    ct:pal("Change the fingerprint from CLI"),
    lmaTestLib:changing_fingerprint_from_cli(?Fingerprint),
    ct:pal("Install LKF"),
    lmaTestLib:install_lkf(),
    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
					?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),

    {ServerRef, ?LicenseState_Enabled, ?GP_monitoring_shall_be_done_1, _, 1000, ?NoLimit_deactive_0} 
	= 
	lmaTestLib:connect_and_subscribe_capcity_over_lihi("CXC4010624", ?GP_AVAILABLE_1),
    lmaTestLib:activate_grace_period(ServerRef, "CXC4010624"),

    timer:sleep(4000),
    ct:pal("Make sure that the GP alarm has changed to MAJOR and GP state to ACTIVATED_EXPIRING"),
    {_, ok} = lmaTestLib:find_mo_from_cli("GracePeriod=CXC4010624", ?LM_MO 
					  ++",CapacityState=CXC4010624", 
					  ["gracePeriodId=\"CXC4010624",
					   "gracePeriodState=ACTIVATED_EXPIRING"], 
					  ?NO_OF_TRIES_XL, ok),
    {_, ok} = lmaTestLib:find_mo_from_cli("FmAlarm=", ?FM_MO, 
					  ["activeSeverity=MAJOR", "minorType=9175051", 
					   "source=\"ManagedElement=1,SystemFunctions=1,Lm=1,"
					   "CapacityState=CXC4010624,GracePeriod=CXC4010624",
					   "activeSeverity=MAJOR"], 
					  ?NO_OF_TRIES, ok),
    %% timer:sleep(2000),
    timer:sleep(4000),
    ct:pal("Make sure that the GP alarm has ceased and GP state changed to EXPIRED for CXC4010719"),
    {_, ok} = lmaTestLib:find_mo_from_cli("GracePeriod=CXC4010624", ?LM_MO 
					  ++",CapacityState=CXC4010624", 
					  ["gracePeriodId=\"CXC4010624",
					   "gracePeriodState=EXPIRED"], 
					  ?NO_OF_TRIES, ok),
    {_, nok} = lmaTestLib:find_mo_from_cli("FmAlarm=", ?FM_MO, 
    					  ["minorType=9175051", "source=\"ManagedElement=1,"
    					   "SystemFunctions=1,Lm=1,CapacityState=CXC4010624"
    					   ++",GracePeriod=CXC4010624"], ?NO_OF_TRIES, nok),

    ct:pal("Grace period has exipred, receive a new LcciCapacityLicenseChangeInd were "
	   "noLimit should not be deactivated"),
    {ok, {?LcciCapacityLicenseChangeInd, ?ClientRef, ?LicenseState_Enabled, 
	  ?GP_monitoring_shall_NOT_be_done_0, _ActivationThreshold, 1000, ?NoLimit_deactive_0}} = 
	rct_proxy:receive_proxy(),
    
    ct:pal("Try to activate GP again on CXC4010624 should get disconnect"),
    ct:pal("Send LcciCapacityLicenseGpActivatedFwd for CXC4010624"),
    rct_proxy:send_proxy(node1, lihi1, ?LcciCapacityLicenseGpActivatedFwd, 
			 {ServerRef, ?ClientRef, "CXC4010624"}),
    {ok, {?LcciCapacityLicenseDisconnectInd, _Client}} = rct_proxy:receive_proxy(),
    
    {_, ok} = lmaTestLib:find_mo_from_cli("GracePeriod=CXC4010624", ?LM_MO 
    					  ++",CapacityState=CXC4010624", 
    					  ["gracePeriodId=\"CXC4010624",
    					   "gracePeriodState=EXPIRED"], 
    					  ?NO_OF_TRIES, ok),
    
    ok = lmaTestLib:unsubscribe_capcity_over_lihi("CXC4010624", ServerRef),
    ct:pal("Test case completeted OK!"),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Testcases that changes a value for a capacity license and cehcks that the MOs
%% are updated correctly.<br/>
%% NodeUC448.A3: Capacity Change
%% @spec change_capacity_value(Config) -> ok
%% @end
%%--------------------------------------------------------------------
change_capacity_value(_Config) ->
    ok = rct_cli:connect(cli),
    ok = rct_rpc:call(rpc_1, lmaGlms, change_test_mode, [on], 100000, noprint),
    case lmaTestLib:check_if_fingerprint_updateable() of
 	ok ->
	    ct:pal("Change the fingerprint from CLI"),
	    lmaTestLib:changing_fingerprint_from_cli(?Fingerprint);
	nok ->
	    ct:pal("Fingerprint is already set")
    end,
    ct:pal("Install LKF"),
    lmaTestLib:install_lkf(),
    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
					?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),
    {_, ok} = lmaTestLib:find_mo_from_cli("CapacityState=CXC4011957", ?LM_MO, 
					  ["keyId=\"CXC4011957", "grantedCapacityLevel=7",
					   "value=7"], ?NO_OF_TRIES, ok),
    {_, ok} = lmaTestLib:find_mo_from_cli("CapacityKey=CXC4011957", ?LM_MO, 
					  ["keyId=\"CXC4011957", "grantedCapacityLevel=7",
					   "value=7"], ?NO_OF_TRIES, ok),
    
    {ServerRef, ?LicenseState_Enabled, ?GP_monitoring_shall_be_done_1, _, 7, 
     ?NoLimit_deactive_0} = 
	lmaTestLib:connect_and_subscribe_capcity_over_lihi("CXC4011957", ?GP_AVAILABLE_1),

    ct:pal("Install LKF with changed capacity value and check that the MOs are updated"),
    JenkinsNode=lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
    TEST_DIR = lmaTestLib:get_test_dir(),
    {ok,_} = file:copy(TEST_DIR++"test2.xml", ?PROJ_DIR++JenkinsNode++"/test2.xml"),
    lmaTestLib:install_lkf(?PROJ_DIR, "test2.xml"),
    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
					?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),
    ct:pal("Receive LcciCapacityLicenseChangeInd"),
    {ok, {?LcciCapacityLicenseChangeInd, ?ClientRef, ?LicenseState_Enabled, 
	  ?GP_monitoring_shall_be_done_1, _ActivationThreshold, 12, ?NoLimit_deactive_0}} = 
	rct_proxy:receive_proxy(),
    {_, ok} = lmaTestLib:find_mo_from_cli("CapacityState=CXC4011957", ?LM_MO, 
					  ["keyId=\"CXC4011957", "grantedCapacityLevel=12",
					   "value=12"], ?NO_OF_TRIES, ok),
    {_, ok} = lmaTestLib:find_mo_from_cli("CapacityKey=CXC4011957", ?LM_MO, 
					  ["keyId=\"CXC4011957", "grantedCapacityLevel=12",
					   "value=12"], ?NO_OF_TRIES, ok),
    ok = unsubscribe_capcity_over_lihi("CXC4011957", ServerRef),
    ct:pal("Test case completeted OK!"),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Testcases that checks that a capcity license can expires anf that the MOs 
%% are updated correctly.<br/>
%% NodeUC449.A3: License Capacity Expires
%% @spec capacity_expires(Config) -> ok
%% @end
%%--------------------------------------------------------------------
capacity_expires(_Config) ->
    ok = rct_cli:connect(cli),
    ok = rct_rpc:call(rpc_1, lmaGlms, change_test_mode, [on], 100000, noprint),
    _ = lmaTestLib:connect_and_subscribe_capcity_over_lihi("CXC4010608", ?GP_AVAILABLE_1),
    timer:sleep(3000),
    case lmaTestLib:check_if_fingerprint_updateable() of
 	ok ->
	    ct:pal("Change the fingerprint from CLI"),
	    lmaTestLib:changing_fingerprint_from_cli(?Fingerprint);
	nok ->
	    ct:pal("Fingerprint is already set")
    end,
    ct:pal("Install LKF were CXC4010608 soon will expire"),
    JenkinsNode=lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
    TEST_DIR = lmaTestLib:get_test_dir(),
    {ok,_} = file:copy(TEST_DIR++"test2.xml", ?PROJ_DIR++JenkinsNode++"/test2.xml"),
    lmaTestLib:install_lkf(?PROJ_DIR, "test2.xml"),
    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
					?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),
    {_ServerRef, ?LicenseState_Enabled, ?GP_monitoring_shall_be_done_1, _,  _CapacityValue, 
     ?NoLimit_deactive_0} = 
	lmaTestLib:connect_and_subscribe_capcity_over_lihi("CXC4010608", ?GP_AVAILABLE_1),
    ct:pal("Wait until the license exipes..."),
    timer:sleep(15000),
    ct:pal("Receive LcciCapacityLicenseChangeInd"),
    {ok, {?LcciCapacityLicenseChangeInd, ?ClientRef, ?LicenseState_Disabled, 
	  ?GP_monitoring_shall_NOT_be_done_0, _ActivationThreshold, _, ?NoLimit_deactive_0}} = 
	rct_proxy:receive_proxy(),
    %% Can only work if EU is not activated
    ct:pal("Check that the License Key Not Available alarm is visible"),
    {_, ok} = lmaTestLib:find_mo_from_cli("FmAlarm=", ?FM_MO, 
					  ["minorType=9175047","source=\"ManagedElement=1,"
					   "SystemFunctions=1,Lm=1,CapacityState=CXC4010608"], 
					  ?NO_OF_TRIES, ok),
    ct:pal("Test case completeted OK!"),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Testcases check that if the featureState is the same after a new LKF is installed.<br/>
%% @spec check_featureState(Config) -> ok
%% @end
%%--------------------------------------------------------------------
check_featureState(_Config) ->
    ok = rct_cli:connect(cli),
    ok = rct_rpc:call(rpc_1, lmaGlms, change_test_mode, [on], 100000, noprint),
    case lmaTestLib:check_if_fingerprint_updateable() of
 	ok ->
	    ct:pal("Change the fingerprint from CLI"),
	    lmaTestLib:changing_fingerprint_from_cli(?Fingerprint);
	nok ->
	    ct:pal("Fingerprint is already set")
    end,
    ct:pal("Install LKF"),
    lmaTestLib:install_lkf(),
    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
					?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),
    
    ct:pal("Change the featureState to ACTIVATED for FeatureState=CXC4011921"),
    lmaTestLib:change_feature_state("ManagedElement=1,SystemFunctions=1,"
				    "Lm=1,FeatureState=CXC4011921", "ACTIVATED"),
    
    ct:pal("Check from CLI that the featureState has changed"),
    ok = lmaTestLib:check_data_from_cli(["featureState=ACTIVATED", "licenseState=ENABLED", 
					 "serviceState=OPERABLE"], 
					"ManagedElement=1,SystemFunctions=1,"
					"Lm=1,FeatureState=CXC4011921", ?NO_OF_TRIES),

    ct:pal("Install LKF were CXC4011921 has changed"),
    JenkinsNode=lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
    TEST_DIR = lmaTestLib:get_test_dir(),
    {ok,_} = file:copy(TEST_DIR++"test2.xml", ?PROJ_DIR++JenkinsNode++"/test2.xml"),
    lmaTestLib:install_lkf(?PROJ_DIR, "test2.xml"),
    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
					?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),

    ok = lmaTestLib:check_data_from_cli(["featureState=ACTIVATED", "licenseState=ENABLED", 
					 "serviceState=OPERABLE"], 
					"ManagedElement=1,SystemFunctions=1,"
					"Lm=1,FeatureState=CXC4011921", ?NO_OF_TRIES),
    ct:pal("Test case completeted OK!"),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Testcases check that if a restart is due to failsafe that correct LKF is 
%%  re-installed on the node.<br/>
%% @spec failsafe_restart(Config) -> ok
%% @end
%%--------------------------------------------------------------------
failsafe_restart(_Config) ->
    ok = rct_cli:connect(cli),
    ok = rct_rpc:call(rpc_1, lmaGlms, change_test_mode, [on], 100000, noprint),
    case lmaTestLib:check_if_fingerprint_updateable() of
 	ok ->
	    ct:pal("Change the fingerprint from CLI"),
	    lmaTestLib:changing_fingerprint_from_cli(?Fingerprint);
	nok ->
	    ct:pal("Fingerprint is already set")
    end,
    ct:pal("Install LKF"),
    lmaTestLib:install_lkf(),
    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
					?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),
    "1001" = get_seq_no_attribute(),
    ok = set_failsafe_attribute(timeoutLength, "10"),
    ok = activate_failsafe(),
    ct:pal("Install LKF with changed higher sequence no then the old one"),
    lmaTestLib:install_lkf(?PROJ_DIR, "test.xml"),
    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
					?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),
    "1002" = get_seq_no_attribute(),
    ct:pal("Wait for the node to restart...."),
     timer:sleep(20000),
    lmaTestLib:wait_for_netconf_started(),

    ct:pal("Restart node done"),
    {ok, client_started} = rct_proxy:start_proxy(node1, lihi1, ?LIHI),
    "1001" = get_seq_no_attribute(),
    ct:pal("Test case completeted OK!"),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Testcases check that if a restart is due to failsafe that correct LKF is 
%%  re-installed on the node.<br/>
%% @spec load_test_mom(Config) -> ok
%% @end
%%--------------------------------------------------------------------
load_test_mom(_Config) ->
    ok = rct_cli:connect(cli),
    case  os:getenv("SIM_OR_TARGET") of
	"sim" ->
	    ct:pal("AI is not perfomed for the simulated env, test mom will return false"),
	    false = rct_rpc:call(rpc_1, lmaI, load_test_mom, [], 100000, noprint),

	    ct:pal("Install LKF that includes test MOM CXC4011959"),
	    JenkinsNode = lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
	    TEST_DIR = lmaTestLib:get_test_dir(),
	    {ok,_} = file:copy(TEST_DIR++"test2.xml", ?PROJ_DIR++JenkinsNode++"/test2.xml"),
	    lmaTestLib:install_lkf(?PROJ_DIR, "test2.xml"),
	    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
	    					?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),
	    
	    ct:pal("Test mom will still return false since checks are only made during AI"),
	    false = rct_rpc:call(rpc_1, lmaI, load_test_mom, [], 100000, noprint);
	"target" ->
	    true = rct_rpc:call(rpc_1, lmaI, load_test_mom, [], 100000, noprint),

	    %% TODO, fix with new LMF.
	    ct:pal("Install LKF that doesn't includes test MOM CXC4011959"),
	    
	    lmaTestLib:install_lkf(),
	    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
						?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),
	    false = rct_rpc:call(rpc_1, lmaI, load_test_mom, [], 100000, noprint),

	    ct:pal("Test case completeted OK!")
    end,
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Testcases check that the embargo liceses (IP Sec) should not be activated during 
%% emergency inlock.<br/>
%% @spec embargo_ipsec(Config) -> ok
%% @end
%%--------------------------------------------------------------------
embargo_ipsec(_Config) ->
    ok = rct_cli:connect(cli),

    case os:getenv("SIM_OR_TARGET") of
	"sim" ->
	    ct:pal("No checks on embargo CXC for simulated env");
	"target" ->

	    %% Move the LKF stored at the node
	    RcsDir = rct_rpc:call(rpc_1, sysEnv, rcs_dir, [], 100000, noprint),
	    NlDir = rct_rpc:call(rpc_1, filename, join, [RcsDir, "networkloader"], 100000, noprint),
	    rct_rpc:call(rpc_1, file, rename, 
			 [NlDir++"/licensingKeyFile.xml",
			  NlDir++"/licensingKeyFile.xml.moved"], 100000, noprint),

	    ct:pal("Reinstall"),
	    rct_rpc:call(rpc_1, sysNetloader,coli_reinstall, [asdf], 10000),
	    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
	    net_kernel:disconnect(ErlNode),
	    ok = rct_cli:disconnect(cli),

	    lmaTestLib:poll_reinstall(nc1),
	    lmaTestLib:wait_for_netconf_started(),
	    {ok, client_started} = rct_proxy:start_proxy(node1, lihi1, ?LIHI),
	    ct:pal("LIHI proxy started"),
	    ok = rct_cli:connect(cli),

	    case lmaTestLib:check_data_from_cli(["activationsLeft=0"], ?EU_MO, 1) of
		ok ->
		    ct:pal("Emergacy unlock has already been activated 2 times");
		nok ->
		    ok = rct_rpc:call(rpc_1, lmaGlms, change_test_mode, [on], 100000, noprint),
		    ct:pal("Change the fingerprint from CLI"),
		    lmaTestLib:changing_fingerprint_from_cli(?Fingerprint),
		    ct:pal("Install LKF where the IP sec licenses (CXC4040004) has been removed"),
		    JenkinsNode = lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
		    TEST_DIR = lmaTestLib:get_test_dir(),
		    {ok,_} = file:copy(TEST_DIR++"test2.xml",?PROJ_DIR++JenkinsNode++"/test2.xml"),
		    lmaTestLib:install_lkf(?PROJ_DIR, "test2.xml"),
		    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
							?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),
		    ct:pal("Subscribe for IP sec and another license"),
		    {ok, ServerRef} = 
			lmaTestLib:connect_and_subscribe_feature_over_lihi("CXC654321"),
		    {ok, ServerRef2} = 
			lmaTestLib:connect_and_subscribe_feature_over_lihi("CXC4040004"),
		    
		    ct:pal("Activate Emergacy unlock"),
		    rct_cli:send(cli, ?EU_MO),
		    rct_cli:send(cli,"activate"),
		    rct_cli:send(cli, "top"),
		    ok = lmaTestLib:check_data_from_cli(["activationState=ACTIVATED"], ?EU_MO, 
							?NO_OF_TRIES),
		    
		    ct:pal("Recieve LiceseState Enabled for CXC654321 and timeout for IP sec"),
		    {ok, {?LfciFeatureLicenseChangeInd, ?ClientRef, _FeatureState, 
			  ?LicenseState_Enabled, "CXC654321"}} = rct_proxy:receive_proxy(),
		    
		    {error,timeout} = rct_proxy:receive_proxy(),
		    
		    ct:pal("Cleanup - Install correct LKF"),
		    lmaTestLib:unsubscribe_capcity_over_lihi("CXC654321", ServerRef),
		    lmaTestLib:unsubscribe_capcity_over_lihi("CXC4040004", ServerRef2),
		    
		    lmaTestLib:install_lkf(),
		    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
							?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),
		    ok = rct_rpc:call(rpc_1, file, rename, 
				      [NlDir++"/licensingKeyFile.xml.moved",
				       NlDir++"/licensingKeyFile.xml"], 100000, noprint)
	    end
    end,
    ok.
 
%%--------------------------------------------------------------------
%% @doc
%% Testcases for the license capacity control interface of LIHI.<br/>
%% <pre> 
%% slogan=Install capacity license, NCL
%% useCase=NodeUC448.A4
%% requirementId=
%% non-requirement=
%% IWD= 
%% addTR= 
%% </pre>
%% <pre> 
%% slogan=GLMS CC can parse new ELIM tag in LKF
%% useCase=CS-LIC_FU_RBS-TCU-OSS_1402:01389
%% requirementId=
%% non-requirement=
%% IWD= 
%% addTR= 
%% </pre>
%% <pre> 
%% slogan=Grace period not available for NCL capacity licenses
%% useCase=CS-LIC_FU_RBS-TCU-OSS_1402:01390
%% requirementId=
%% non-requirement=
%% IWD= 
%% addTR= 
%% </pre>
%% <pre> 
%% slogan=Update value of attribute reflecting the licensed capacity for NCL case
%% useCase=CS-LIC_FU_RBS-TCU-OSS_1402:01391
%% requirementId=
%% non-requirement=
%% IWD= 
%% addTR= 
%% </pre>
%% <pre> 
%% slogan=Terminate grace period when license is upgraded to NCL from a priced or boundary
%% useCase=CS-LIC_FU_RBS-TCU-OSS_1402:01392
%% requirementId=
%% non-requirement=
%% IWD= 
%% addTR= 
%% </pre>
%% @end
%% @spec license_capacity_with_ncl(Config) -> ok
%% @end
%%--------------------------------------------------------------------
license_capacity_with_ncl(_Config) ->
    ok = rct_cli:connect(cli),
    ok = rct_rpc:call(rpc_1, lmaGlms, change_test_mode, [on], 100000, noprint),
    case lmaTestLib:check_if_fingerprint_updateable() of
 	ok ->	    
	    ct:pal("Change the fingerprint from CLI"),
	    lmaTestLib:changing_fingerprint_from_cli(?Fingerprint);
	nok ->
	    ct:pal("Fingerprint is already set")
    end,
    ct:pal("Install LKF"),
    lmaTestLib:install_lkf(),
    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
					?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),
    {ServerRef, ?LicenseState_Enabled, ?GP_monitoring_shall_be_done_1, _, 1000, 
     ?NoLimit_deactive_0} =  
	lmaTestLib:connect_and_subscribe_capcity_over_lihi("CXC4010623", ?GP_AVAILABLE_1),

    ct:pal("Activate GP for CXC4010623 and make sure that the MO is created"),
    lmaTestLib:activate_grace_period(ServerRef, "CXC4010623"),
    
    ct:pal("Install LKF with NCL"),
    JenkinsNode=lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
    TEST_DIR = lmaTestLib:get_test_dir(),
    {ok,_} = file:copy(TEST_DIR++"ncllkf.xml", ?PROJ_DIR++JenkinsNode++"/ncllkf.xml"),
    lmaTestLib:install_lkf(?PROJ_DIR, "ncllkf.xml"),
    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
					?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),

    ct:pal("Receive LcciCapacityLicenseChangeInd for CXC4010623"),
    {ok, {?LcciCapacityLicenseChangeInd, ?ClientRef, ?LicenseState_Enabled, 
	  ?GP_monitoring_shall_NOT_be_done_0, _ActivationThreshold, CapacityValue, 
	  ?NoLimit_active_1}} = rct_proxy:receive_proxy(),
    ct:pal("CapacityValue: ~p", [CapacityValue]),

    {_, ok} = lmaTestLib:find_mo_from_cli("CapacityState=CXC4010623", ?LM_MO, 
    					  ["keyId=\"CXC4010623",
					   "grantedCapacityLevel=-1",
					   "value=-1"], 
    					  ?NO_OF_TRIES, ok),
    {_, ok} = lmaTestLib:find_mo_from_cli("CapacityKey=CXC4010623", ?LM_MO, 
    					  ["grantedCapacityLevel=-1",
					   "value=-1"], 
    					  ?NO_OF_TRIES, ok),
   
    ct:pal("Check that the GP MO for CXC4010623 has been deleted"),
    {_, nok} = lmaTestLib:find_mo_from_cli("GracePeriod=CXC4010623", ?LM_MO 
					   ++",CapacityState=CXC4010623", 
					   ["gracePeriodId=\"CXC4010623"], 
					   ?NO_OF_TRIES, nok),
    ct:pal("Check that the GP alarm for CXC4010623 is not visible"),
    {_, nok} = 
	lmaTestLib:find_mo_from_cli("FmAlarm=", ?FM_MO, 
				    ["minorType=9175051", "source=\"ManagedElement=1,"
				     "SystemFunctions=1,Lm=1,CapacityState=,CXC4010623"
				     "GracePeriod=CXC4010623"], ?NO_OF_TRIES, nok),


    {ServerRef2, ?LicenseState_Enabled, ?GP_monitoring_shall_NOT_be_done_0, _, -1, 
     ?NoLimit_active_1} = 
    	lmaTestLib:connect_and_subscribe_capcity_over_lihi("CXC1723578/0000", ?GP_AVAILABLE_1, 
							   ?ClientRef_2),
 
    {_, ok} = lmaTestLib:find_mo_from_cli("CapacityState=CXC1723578/0000", ?LM_MO, 
    					  ["keyId=\"CXC1723578/0000",
    					   "description=\"Description of the capacity",
					  "grantedCapacityLevel=-1",
					   "value=-1"], 
    					  ?NO_OF_TRIES, ok),
    {_, ok} = lmaTestLib:find_mo_from_cli("CapacityKey=CXC1723578/0000", ?LM_MO, 
    					  ["grantedCapacityLevel=-1",
					   "value=-1"], 
    					  ?NO_OF_TRIES, ok),
    
    ct:pal("Check that the GP MO is NOT created for CXC1723578/0000"),
    {_, nok} = lmaTestLib:find_mo_from_cli("GracePeriod=CXC1723578/0000", ?LM_MO 
					  ++",CapacityState=CXC1723578/0000", 
					  ["gracePeriodId=\"CXC1723578/0000"], 
					  ?NO_OF_TRIES, nok),

    ct:pal("Check that the GP alarm is not visible"),
    {_, nok} = 
	lmaTestLib:find_mo_from_cli("FmAlarm=", ?FM_MO, 
				    ["minorType=9175051", "source=\"ManagedElement=1,"
				     "SystemFunctions=1,Lm=1,CapacityState=CXC1723578/0000,"
				     "GracePeriod=CXC1723578/0000"], ?NO_OF_TRIES, nok),
    ok = unsubscribe_capcity_over_lihi("CXC4011484", ServerRef),
    ok = unsubscribe_capcity_over_lihi("CXC1723578/0000", ServerRef2, ?ClientRef_2),
    ok.  

unsubscribe_capcity_over_lihi(CXC, ServerRef) ->
    unsubscribe_capcity_over_lihi(CXC, ServerRef, ?ClientRef).

unsubscribe_capcity_over_lihi(CXC, ServerRef, Client) ->
    ct:pal("Send LcciCapacityLicenseUnsubscribeReq to GLMS over LIHI for " ++ CXC),
    {ok, capacity_license_unsubscribe_req} = 
    	rct_proxy:send_proxy(node1, lihi1, ?LcciCapacityLicenseUnsubscribeReq, 
			     {ServerRef, Client, CXC}),
    ct:pal("Wait for LcciCapacityLicenseUnsubscribeCfm from GLMS"),
    {ok, {_LcciCapacityLicenseUnsubscribeCfm, Client}} = rct_proxy:receive_proxy(),
    ok.

activate_failsafe() ->
    Activate = {'ManagedElement',
		[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		[{managedElementId,[],["1"]},
		 {'SystemFunctions',
		  [{systemFunctionsId,[],["1"]},
		   {'BrM',
		    [{brMId,[],["1"]},
		     {'BrmBackupManager',
		      [{brmBackupManagerId,[],["1"]},
		       {'BrmFailsafeBackup',
			[{brmFailsafeBackupId, [], ["1"]},
			 {'activate',[]}]}]}]}]}]},
    ct:pal("Executing action activate"),
    {ok, ActionResponse} = netconf(action, [nc1, Activate]),
    case lmaTestLib:extract_element(returnValue, ActionResponse) of
	{ok, {returnValue, _, ["0"]}} ->
	    ok;
	{ok, {returnValue, _, [ReturnValue]}} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)~n",[Error, ReturnValue]),
	    ct:fail(Error)
    end.

decode_return_value("0") -> actionComplete;
decode_return_value("1") -> nameValidationFailure;
decode_return_value("2") -> duplicateName;
decode_return_value("3") -> housekeepingRequired;
decode_return_value("4") -> backupNotFound;
decode_return_value("97") -> functionBusy;
decode_return_value("98") -> missingParameter;
decode_return_value("99") -> softwareFault;
decode_return_value(Value) ->
    ct:fail({unknown_return_value, Value}).

get_seq_no_attribute() ->
    Get =
    	{'ManagedElement',
    	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    	 [{managedElementId,[],["1"]},
    	  {'SystemFunctions',
    	   [{systemFunctionsId,[],["1"]},
    	    {'Lm',
    	     [{lmId,[],["1"]},
    	      {'KeyFileManagement',
    	       [{keyFileManagementId,[],["1"]},
    		{'KeyFileInformation',
    		 [{keyFileInformationId,[],["1"]},
    		  {sequenceNumber, []}]}]}]}]}]},
    {ok, Res} = netconf(get, [nc1, Get]),
    {ok, {_, _, [Value]}} = lmaTestLib:extract_element(sequenceNumber, Res),
    Value.

set_failsafe_attribute(Attribute, Value) ->
    EditConfig =
    	{'ManagedElement',
    	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    	 [{managedElementId,[],["1"]},
    	  {'SystemFunctions',
    	   [{systemFunctionsId,[],["1"]},
    	    {'BrM',
    	     [{brMId,[],["1"]},
    	      {'BrmBackupManager',
    	       [{brmBackupManagerId,[],["1"]},
    		{'BrmFailsafeBackup',
    		 [{brmFailsafeBackupId,[],["1"]},
    		  {Attribute, [Value]}]}]}]}]}]},
    netconf(edit_config, [nc1, running, EditConfig]).

	   
netconf(F, A) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    case apply(ct_netconfc, F, A) of
	ok ->
	    ok = ct_netconfc:close_session(nc1),
	    ok;
	{ok, Res} ->
	    ok = ct_netconfc:close_session(nc1),
	    {ok, Res};
	Other ->
	    ct:fail(Other)
    end.







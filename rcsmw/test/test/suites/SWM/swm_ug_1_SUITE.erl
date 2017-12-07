%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_ug_1_SUITE.erl %
%%% @author eivomat
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R3A/R4A/R5A/R6A/R7A/R9A/5
%%%
%%% @doc == Test Suite for Upgrade Mecthanism, CS MW configured before UG. To UP is same as installed except rev is stepped in cxs-up xml. ==
%%% <br/><br/>
%%% @end

-module(swm_ug_1_SUITE).
-vsn('/main/R3A/R4A/R5A/R6A/R7A/R9A/5').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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
%%% R2A/2      2014-03-10 etxivri     Created
%%% R2A/3      2014-04-08 erarafo     Deprecation warning
%%% R2A/4      2014-05-23 etxivri     Changed commit to confirm.
%%% R2A/6      2014-09-25 etxivri     Update when get the UP label to be used.
%%% R3A/3      2015-01-28 etxmlar     Start adding UG tests for Lm=1
%%% R3A/4      2015-01-29 etxmlar     Continue adding UG tests for Lm=1
%%% R3A/5      2015-02-19 etxmlar     Continue adding UG tests for Pm=1
%%% R3A/6      2015-04-29 etxmlar     Continue adding UG tests for SwM=1
%%% R3A/7      2015-06-02 etxmlar     First suite version ready
%%% R3A/8      2015-06-03 etxmlar     Updated fingerprint check after Ug
%%% R3A/9      2015-06-15 etxmlar     Added CXP UP build 
%%% R4A/1      2015-06-26 etxmlar     Added more in CERT
%%% R4A/2      2015-06-29 etxmlar     Update expected answer when asking
%%%                                   jenkins config for installed_type on tcu
%%% R4A/3      2015-06-30 etxmlar     Changed ESI NoOfTries to 40
%%% R4A/4      2015-06-30 etxmlar     Forgotten comment
%%% R5A/1      2015-11-27 etxmlar     Updated maxStoredManualBackups=18 for R5A
%%% R5A/2      2015-12-07 etxmlar     Increased time to wait for fetch_esi
%%% R5A/3      2016-01-15 etxivri     Update due to new lkf is used in 16B.
%%% R6A/1      2016-04-28 etxpejn     Temp. removed checks for EnrollmentServer
%%% R6A/3      2016-04-28 etxmlar     Corrected the checks for EnrollmentServer
%%% R6A/4      2016-08-18 etxkols     Git migration requires that CC paths is not used 
%%% R6A/5      2016-09-05 ekurnik     HV14974: fallbackTimer set to 1800 after upgrade
%%% R7A/1      2016-10-13 etxivri     A try to make it more robust.
%%% R9A/1      2017-02-03 etxmlar     Updated Cmpv2Host parameter
%%% R9A/2      2017-03-06 etxivir     Update to new behaviour. 
%%%                                   fallback timer value will be kept after ug.
%%% R9A/5      2017-03-21 eivomat     Cannot set both Fingerprint and Cert
%%%                                   on EnrollAuth due to fix for HV70387.
%%% ----------------------------------------------------------

%-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).
-export([create/1,
	 prepare/1,
	 verify/1,
	 activate/1,
	 confirm/1,
	 remove/1,
	 mod_to_up/1,
	 pre_ug/1,
	 after_ug/1
	]).

-define(SftpHost, swm_test_lib:get_sftp_host()).
-define(SftpUser , swm_test_lib:get_sftp_user()).
-define(SftpPassword , swm_test_lib:get_sftp_password()).

-define(NC_Session, nc1).
-define(CLI_Session, cli).
-define(UG_Session, ug1).

-define(DC_UP_NAME, "/DC-CXP*.tgz").

%% LM defines
-define(LiKeyFault, "9175046").  %% Key file fault in Managed Element, 
                                 %% minorType=9175046
-define(EmUnlockResetKeyRequired, "9175048"). %% minorType=9175048
-define(LM_LDN, "ManagedElement=1,SystemFunctions=1,Lm=1").
-define(EU_LDN, "ManagedElement=1,SystemFunctions=1,Lm=1,EmergencyUnlock=1").

%% FM defines
-define(FM_LDN, "ManagedElement=1,SystemFunctions=1,Fm=1").

%% PM defines
-define(PM_LDN, "ManagedElement=1,SystemFunctions=1,Pm=1").
-define(PMEVENT_LDN, "ManagedElement=1,SystemFunctions=1,PmEventM=1").

%% SWM defines
-define(SWM_LDN, "ManagedElement=1,SystemFunctions=1,SwM=1").

%% BRM defines
-define(BRM_LDN, "ManagedElement=1,SystemFunctions=1,BrM=1").

%% SEC defines
-define(SEC_LDN, "ManagedElement=1,SystemFunctions=1,SecM=1").
-define(CERT_PATH, rct_cc_git_path:find("RCS_TOP", [ "CERT/CERT_CNX9013071/test/suites/certm_SUITE_data/", "CERT/test/suites/certm_SUITE_data/"])).

%% SYS defines
-define(SYS_LDN, "ManagedElement=1,SystemFunctions=1,SysM=1").

%% LOG defines
-define(LOG_LDN, "ManagedElement=1,SystemFunctions=1,LogM=1").


%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf.
%% @end
%%--------------------------------------------------------------------
suite() ->
    case check_if_vc_board() of
	"yes" -> [{timetrap, {hours, 2}},
		  {ct_hooks, [{rct_htmllink,[]},
			      {rct_rpc, rpc_1},
			      {rct_upgrade,ug1},
			      %% {cth_conn_log,[]},
			      %% {rct_core,[]},
			      {rct_cli, {cli,
			      		 [{user, "SysAdminTest"}, 
			      		  {password, "SysAdminTest"},
			      		  manual_connect]}},			    
			      {rct_netconf, {nc1, man_auth}},
			      {rct_rs232,console}
			     ]}];
	_  ->
	    [{timetrap, {hours, 2}},
	     {ct_hooks, [{rct_htmllink,[]},
			 {rct_rpc, rpc_1},
			 {rct_upgrade,ug1},
			 %%{cth_conn_log, []},
			 {rct_core,[]},
			 {rct_logging, {upgrade,
					[{erlang,{["ERROR REPORT",
						   "CRASH REPORT"],
						  []
						 }}]}},
			 {rct_cli, {cli, [manual_connect]}},
			 {rct_netconf, nc1}]}]
    end.


%% @hidden
init_per_suite(Config) ->

    %% Clean up before testsuite starts, reinstall node
    ok= post_ug_lib:reinstall_rbs(rpc_1, ?NC_Session),

    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
    [{sec_board, Secure_board}|Config].

%% @hidden
end_per_suite(_Config) ->
    case check_if_vc_board() of
	"yes" -> 
	    continue;
	_  ->
	    swm_test_lib:erase_housekeeping_delay_variable(rpc_1)
    end,
    
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.

%% @hidden
end_per_testcase(after_ug, _Config) ->

    %% Clean up, reinstall node (nor for secure boards)
    ok = post_ug_lib:reinstall_rbs(rpc_1, ?NC_Session),
    ok;
%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.

%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() ->
    [mod_to_up,
     pre_ug, 
     create, 
     prepare, 
     verify, 
     activate, 
     confirm,
     after_ug
    ].

%%%--------------------------------------------------------------------
%%% @doc
%%% Configure CS MW before upgrade via CLI. <br/>
%%% @spec pre_ug(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
pre_ug(Config) ->

    ct:pal("Config:~n~p~n",[Config]),

    %% Get managedElementId
    MeId = swm_test_lib:get_me_id(?NC_Session),
    %% MeId = proplists:get_value(meId, Config),

    ok = rct_cli:connect(?CLI_Session),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% License Management (ManagedElement=1,SystemFunctions=1,Lm=1)   
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %% Check License Key File Fault Alarm (Critical) exist
    case post_ug_lib:check_if_fingerprint_updateable() of
    	ok->
    	    wait_for_exp_alarm_to_exist(?NC_Session, MeId, ?LiKeyFault),
    	    post_ug_lib:create_and_install_lkf();
    	nok->
    	    continue
    end,
    
    %% Check License Key File Fault Alarm (Critical) NOT exist
    wait_for_exp_alarm_to_cease(?NC_Session, MeId, ?LiKeyFault),

    %% Change FeatureState to ACTVATED
    ct:pal("Change some featureState to ACTIVATED"),
    post_ug_lib:change_attribute_from_cli("featureState = ACTIVATED", 
					  ?LM_LDN ++ ",FeatureState=CXC4010320"),
    post_ug_lib:change_attribute_from_cli("featureState = ACTIVATED", 
					  ?LM_LDN ++ ",FeatureState=CXC4010511"),

    
    %% Activate Emergency unlock
    ct:pal("Activate Emergency unlock"),
    post_ug_lib:change_mo_from_cli(?EU_LDN),   

    %% For the future: Check that following works. Add a "fake" application 
    %% which asks for a license before and after upggrade 

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Fault Management (ManagedElement=1,SystemFunctions=1,Fm=1)   
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %% Change Attribute, heartbeatInterval and configuredSeverity
    ct:pal("Change heartbeatInterval and configuredSeverity"),
    post_ug_lib:change_attribute_from_cli("heartbeatInterval=30", ?FM_LDN),
    post_ug_lib:change_attribute_from_cli(
      "configuredSeverity=MINOR", 
      ?FM_LDN ++ ",FmAlarmModel=1,FmAlarmType=EnclosureDoorOpen"),
    
    %% For the future: Check that following works. Add a "fake" application 
    %% which add an alarm before upgrade and check that it "survives" the upgrade.
    %% Perhaps also include a check that heartbeatIntervall change works

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Performance Management (ManagedElement=1,SystemFunctions=1,Pm=1)   
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %%Create user-defined PM jobs
    ct:pal("Create PmJob=1, PmJob=2, PmJob=3 and PmJob=4"),

    %%Job 1
    post_ug_lib:create_pmjob_from_cli(?PM_LDN ++ ",PmJob=1",
     				      "MeasurementReader=1",
     				      "measurementSpecification", 
     				      "groupRef="
				      "ManagedElement=1,SystemFunctions=1,Pm=1,PmGroup=Group1"),
    
    %%Job 2
    post_ug_lib:create_pmjob_from_cli(?PM_LDN ++ ",PmJob=2",
				      ["jobPriority=LOW", "jobType=THRESHOLDJOB",
				       "requestedJobState=STOPPED"],
				      "MeasurementReader=2",
				      "measurementSpecification", 
				      "groupRef="
				      "ManagedElement=1,SystemFunctions=1,Pm=1,PmGroup=Group2"),
    
    %%Job 3
    post_ug_lib:create_pmjob_from_cli(?PM_LDN ++ ",PmJob=3",
				      "MeasurementReader=1",
				      "measurementSpecification", 
				      "groupRef="
				      "ManagedElement=1,SystemFunctions=1,Pm=1,PmGroup=ROPGroup2"),

    %%Job 4
    post_ug_lib:create_pmjob_from_cli(?PM_LDN ++ ",PmJob=4",
				      ["compressionType=GZIP","jobPriority=HIGH"],
				      "MeasurementReader=2",
				      "measurementSpecification", 
				      "groupRef="
				      "ManagedElement=1,SystemFunctions=1,Pm=1,PmGroup=ROPGroupMult"),
    

    EventGroupRef = "eventGroupRef=[\"ManagedElement=1,SystemFunctions=1,PmEventM=1, EventProducer=first,EventGroup=EventGrp1\", \"ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp2\"]",
    
    EventTypeRef = "eventTypeRef=[\"ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp1,EventType=EvType1\", \"ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp2,EventType=EvType2\"]",
    
    %%Create PmEventJob
    ct:pal("Create EventJob=6"),
    
    RatType = get_rat_type(), 

    case RatType of
    	grat ->
	    post_ug_lib:create_pmevent_from_cli(?PMEVENT_LDN ++ ",EventProducer=Grat",
						"EventJob=6",
						["fileOutputEnabled=true",
						 "fileCompressionType=GZIP",
						 "reportingPeriod=FIFTEEN_MIN", 
						 "streamOutputEnabled=true",
						 "streamCompressionType=GZIP",
						 "streamDestinationIpAddress=123.1.2.3",
						 "streamDestinationPort=2205","eventFilter[@1]",
						 "filterName=SingleSelect","filterValue=ue"],
						EventGroupRef,
						EventTypeRef);
    	wrat ->

	    %% NOT IMPLEMENTED YET!!
	    %% post_ug_lib:create_pmevent_from_cli(?PMEVENT_LDN ++ ",EventProducer=Wrat",
	    %% 					"EventJob=6",
	    %% 					["fileOutputEnabled=true",
	    %% 					 "fileCompressionType=GZIP",
	    %% 					 "reportingPeriod=FIFTEEN_MIN", 
	    %% 					 "streamOutputEnabled=true",
	    %% 					 "streamCompressionType=GZIP",
	    %% 					 "streamDestinationIpAddress=123.1.2.3",
	    %% 					 "streamDestinationPort=2205","eventFilter[@1]",
	    %% 					 "filterName=SingleSelect","filterValue=ue"],
	    %% 					EventGroupRef,
	    %% 					EventTypeRef);
	    continue;
	lrat ->
	    post_ug_lib:create_pmevent_from_cli(?PMEVENT_LDN ++ ",EventProducer=Lrat",
						"EventJob=6",
						["fileOutputEnabled=true",
						 "fileCompressionType=GZIP",
						 "reportingPeriod=FIFTEEN_MIN", 
						 "streamOutputEnabled=true",
						 "streamCompressionType=GZIP",
						 "streamDestinationIpAddress=123.1.2.3",
						 "streamDestinationPort=2205","eventFilter[@1]",
						 "filterName=SingleSelect","filterValue=ue"],
						EventGroupRef,
						EventTypeRef);
	tcu ->
	    %% NOT VALID!!
	    continue;
	
	_Else ->
	    post_ug_lib:create_pmevent_from_cli(?PMEVENT_LDN ++ ",EventProducer=first",
						"EventJob=6",
						["fileOutputEnabled=true",
						 "fileCompressionType=GZIP",
						 "reportingPeriod=FIFTEEN_MIN", 
						 "streamOutputEnabled=true",
						 "streamCompressionType=GZIP",
						 "streamDestinationIpAddress=123.1.2.3",
						 "streamDestinationPort=2205","eventFilter[@1]",
						 "filterName=SingleSelect","filterValue=ue"],
						EventGroupRef,
						EventTypeRef)
    end,

    %% For the future:
    %% %%Configure user-defined PM jobs
    %% ct:pal("Configure PmJob=1"),
    %% post_ug_lib:change_attributes_from_cli(?CLI_Session, 
    %% 					   ["jobPriority=LOW","jobType=THRESHOLDJOB",
    %% 					    "requestedJobState=STOPPED"],
    %% 					   ?PM_LDN ++ ",PmJob=1"),
    
    %% ct:pal("Configure PmJob=3"),
    %% post_ug_lib:change_attributes_from_cli(?CLI_Session, 
    %% 					   ["granularityPeriod=ONE_HOUR", "jobPriority=HIGH", 
    %% 					    "reportingPeriod=TWELVE_HOUR"],
    %% 					   ?PM_LDN ++ ",PmJob=3"),
  
    
    %% For the future: Check that following works. Add a "fake" application 
    %% which asks for a pmjobb/event before and after upggrade.

    %% For the future: Add pmjobb with a Predefined appdata file during upgrade

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Software Management (ManagedElement=1,SystemFunctions=1,SwM=1)  
    %% Backup and restore Management (ManagedElement=1,SystemFunctions=1,BrM=1) 
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %% Change fallbackTimer
    ct:pal("Change fallbackTimer"),
    post_ug_lib:change_attribute_from_cli("fallbackTimer=1000", ?SWM_LDN),

    %% Backup and Restore
    ct:pal("Set exportPackageLabelPrefix"),
    post_ug_lib:change_attribute_from_cli("exportPackageLabelPrefix=upgrade_config_test", 
					  ?BRM_LDN),

    %% Housekeeping + autoDelete
    ct:pal("Set housekeeping and autoDelete"),
    post_ug_lib:change_attributes_from_cli(["maxStoredManualBackups=18","autoDelete=DISABLED"],
					   ?BRM_LDN++",BrmBackupManager=1,BrmBackupHousekeeping=1"),
    
    %% For the future:
    %% autoDelete=DISABLED -> ENABLE/DISABLE
    %% Check? If set to DISABLED, no backup is removed. 
    %% Instead, when action createBackup() is invoked, it returns a failure parameter. 

 
    %% Configure Automatic Backup
    
    %% Configure Backup Scheduler"
    ct:pal("Configure Backup Scheduler"),
    {ok, _} = post_ug_lib:change_attributes_from_cli(
		["maxStoredScheduledBackups=4",
		 "scheduledBackupName=BACKUP_CONFIGURATION_TEST"],
		?BRM_LDN++",BrmBackupManager=1,BrmBackupScheduler=1"),
	
    %% Configure Single Scheduled Backup Event
    ct:pal("Configure Single Scheduled Backup Event"),
    {ok, _} = post_ug_lib:create_backup_mo_from_cli("BrmSingleEvent=1", 
						    ["scheduledTime=2030-04-05T02:00:00"], 
						    ?BRM_LDN++",BrmBackupManager=1,BrmBackupScheduler=1"),
    
    %% Configure Periodic Scheduled Backup Event Based on Calendar
    ct:pal("Configure Periodic Scheduled Backup Event Based on Calendar"),
    {ok, _} = post_ug_lib:create_backup_mo_from_cli("BrmCalendarBasedPeriodicEvent=1", 
						    ["dayOfMonth=17","dayOfWeek=THURSDAY",
						     "dayOfWeekOccurrence=THIRD","month=12",
						     "startTime=2030-12-17T02:00:00",
						     "stopTime=2030-12-17T04:00:00",
						     "time=02:00:00"], 
						    ?BRM_LDN++",BrmBackupManager=1,BrmBackupScheduler=1"),
    
    %% Configure Periodic Scheduled Backup Event 
    ct:pal("Configure Periodic Scheduled Backup Event"),
    {ok, _} = post_ug_lib:create_backup_mo_from_cli("BrmPeriodicEvent=1", 
						    ["days=20","hours=12","minutes=10",
						     "startTime=2030-08-17T04:00:00",
						     "stopTime=2030-09-17T04:00:00",
						     "weeks=4"], 
						    ?BRM_LDN++",BrmBackupManager=1,BrmBackupScheduler=1"),


    %% Configure Automatic Export of Scheduled Backup 
    ct:pal("Configure Automatic Export of Scheduled Backup"),
    {ok, _} = post_ug_lib:change_attributes_from_cli(
		["autoExportPassword=\"1:db/ejy4EkZ3jHzU1PuEXktY1504=\"",
		 "autoExportUri=http://www.testweb.se",
		 "autoExport=ENABLED"],                    
		?BRM_LDN++",BrmBackupManager=1,BrmBackupScheduler=1"),
    

    %% Suspend Backup Scheduler
    ct:pal("Suspend Backup Scheduler"),
    {ok, _} = post_ug_lib:change_attribute_from_cli("adminState=LOCKED",                    
						    ?BRM_LDN++",BrmBackupManager=1,BrmBackupScheduler=1"),

    %% For the future:????
    %% %% Resume Backup Scheduler
    %% ct:pal("Resume Backup Scheduler"),
    %% post_ug_lib:change_attributes_from_cli(?CLI_Session,
    %% 					   ["adminState=UNLOCKED"],  
    %% 					   ?BRM_LDN++",BrmBackupManager=1,BrmBackupScheduler=1"),
    

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Security Management (ManagedElement=1,SystemFunctions=1,SecM=1)   
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    ct:pal("Configure O&M Access Point"),

    case RatType of
    	grat ->
    	    %% configure_node_ipv4address_suppot_via_cli(),
	    
    	    %% {ok, _} = post_ug_lib:change_attributes_from_cli(
    	    %% 		["ipv4address=ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=1,AddressIPv4=1", 
    	    %% 		 "dscp=40"
    	    %% 		 %% "netconfPort=2222",
    	    %% 		 %% "sshPort=3333"
    	    %% 		],                   
    	    %% 		?SYS_LDN++",OamAccessPoint=1");
	    continue;
    	wrat ->
    	    %% configure_node_ipv4address_suppot_via_cli(),

    	    %% {ok, _} = post_ug_lib:change_attributes_from_cli(
    	    %% 		["ipv4address=ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=1,AddressIPv4=1", 
    	    %% 		 "dscp=40"
    	    %% 		 %% "netconfPort=2222",
    	    %% 		 %% "sshPort=3333"
    	    %% 		],                   
    	    %% 		?SYS_LDN++",OamAccessPoint=1");  
	    continue;
    	lrat ->
    	    %% configure_node_ipv4address_suppot_via_cli(),

    	    %% {ok, _} = post_ug_lib:change_attributes_from_cli(
    	    %% 		["ipv4address=ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=1,AddressIPv4=1",
    	    %% 		 "dscp=40"
    	    %% 		 %% "netconfPort=2222",
    	    %% 		 %% "sshPort=3333"
    	    %% 		],                   
    	    %% 		?SYS_LDN++",OamAccessPoint=1");  
	    continue;
    	tcu ->
    	    %% configure_node_ipv4address_suppot_via_cli(),
	    
    	    %% {ok, _} = post_ug_lib:change_attributes_from_cli(
    	    %% 		["ipv4address=ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=1,AddressIPv4=1", 
    	    %% 		 "dscp=40"
    	    %% 		 %% "netconfPort=2222",
    	    %% 		 %% "sshPort=3333"
    	    %% 		],                   
    	    %% 		?SYS_LDN++",OamAccessPoint=1");  
	    continue;
	
    	_Else1 ->
    	    {ok, _} = post_ug_lib:change_attributes_from_cli(
    			["ipv4address=ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=1,AddressIPv4=1", 
    			 "dscp=40"
    			 %% "netconfPort=2222",
    			 %% "sshPort=3333"
    			],                   
    			?SYS_LDN++",OamAccessPoint=1")  
    end,
    
    %% Default value
    %% dscp=0 <default>
    %% netconfPort=2022
    %% oamAccessPointId="1"
    %% sshPort=2023
    
    %% Configure CertM
    ct:pal("CertM, set userLabel"),
    {ok, _} = post_ug_lib:change_attribute_from_cli("userLabel=\"Created by swm_ug_1_SUITE\"", 
    						    ?SEC_LDN++",CertM=1"),
    

    %% Configure EnrollmentAuthority 
    ct:pal("Configure EnrollmentAuthority"),

    %% After HV70387 cannot set both Fingerprint and CaCertificate
    %% Use Fingerprint
    EADataDir = ?CERT_PATH,
    EAFile = proplists:get_value(file, Config, "etxasta.pem"),
    EAPath =  filename:join(EADataDir, EAFile),

    [_, FP] = string:tokens(cmd(["openssl x509 -fingerprint -noout -in ", EAPath]), "="),

    EnAFingerprint = string:strip(FP, both, $\n),
    EnrollmentCaFingerprint  = lists:concat(["enrollmentCaFingerprint=\"", EnAFingerprint, "\""]),
    
    {ok, _} = post_ug_lib:create_enrollmentauthority_mo_from_cli(
		"EnrollmentAuthority=1",
		["authorityType=CERTIFICATION_AUTHORITY",
		 "enrollmentAuthorityName=\"C=SE,O=Ericsson,CN=etxmlar\"",
         %% Cannot set reference to an MO that doesn't yet exist
         %%"enrollmentCaCertificate=ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustedCertificate=2",
		 EnrollmentCaFingerprint,
		 "userLabel=\"Created by swm_ug_1_SUITE\""],
		?SEC_LDN++",CertM=1"),
    
    
    %% Configure EnrollmentServerGrpup
    ct:pal("Configure EnrollmentServerGrpup"),
    {ok, _} = post_ug_lib:create_enrollmentservergroup_mo_from_cli(
		"EnrollmentServerGroup=1",
		["userLabel=\"Created by swm_ug_1_SUITE\""],
		?SEC_LDN++",CertM=1"),

    
    %% Configure EnrollmentServer
    ct:pal("Configure EnrollmentServer"),

    [{host, Cmpv2Server}] = ct:get_config(cmpv2_server),
    Cmpv2Host = "http://"++Cmpv2Server++":8180/",

    ESUri = lists:concat(["uri=\"", Cmpv2Host, "\""]),
 
    {ok, _} = post_ug_lib:create_enrollmentserver_mo_from_cli(
    		"EnrollmentServer=1",
    		["enrollmentAuthority="
    		 "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,EnrollmentAuthority=1",
    		 "protocol=CMP",
    		 ESUri,
    		 "userLabel=\"Created by swm_ug_1_SUITE\""],
    		?SEC_LDN++",CertM=1"++",EnrollmentServerGroup=1"),
    

    %% Configure NodeCredential 
    ct:pal("Configure NodeCredential 1, startOnlineEnrollment"),
    {ok, _} = 
    	post_ug_lib:create_nodecredential_mo_from_cli(
	  "NodeCredential=1",
	  ["enrollmentAuthority="
	   "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,EnrollmentAuthority=1",
	   "enrollmentServerGroup="
	   "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,EnrollmentServerGroup=1",
	   "enrollmentTimer=5",
	   "expiryAlarmThreshold=50",
	   "keyInfo=RSA_2048",
	   "renewalMode=AUTOMATIC",
	   "subjectName=\"C=SE,O=Ericsson,CN=etxmlar1\"",
	   "userLabel=\"Created by swm_ug_1_SUITE\""],
	  ?SEC_LDN++",CertM=1"),


    %% startOnlineEnrollment
    %% NOTE! Not possible to implement yet. Need to update cmpv2_server (Andreas). Maybe in the future.
    %% ct:pal("startOnlineEnrollment."),
    %% ct:pal("Action startOnlineEnrollment."),
    %% {ok, _} = post_ug_lib:startonlineenrollment_from_cli("startOnlineEnrollment", "NULL",  
    %% 							 ?SEC_LDN++",CertM=1"++",NodeCredential=1"),

    %%  ok = post_ug_lib:check_progress_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
    %% 						  ?SEC_LDN++",CertM=1"++",NodeCredential=1", 40), %%20 orginal
    

    %% Configure NodeCredential for Offline enrollment when the container file is in the PKCS#12 format
    ct:pal("Configure NodeCredential 2, Offline enrollment PKCS#12 format"),
    {ok, _} = 
    	post_ug_lib:create_nodecredential_mo_from_cli(
	  "NodeCredential=2",
	  ["enrollmentAuthority="
	   "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,EnrollmentAuthority=1",
	   "enrollmentServerGroup="
	   "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,EnrollmentServerGroup=1",
	   "enrollmentTimer=5",
	   "expiryAlarmThreshold=50",
	   "keyInfo=RSA_2048",
	   "subjectName=\"C=SE,O=Ericsson,CN=swm_ug_1_SUITE_pkcs12.ericsson.com\"",
	   "userLabel=\"Created by swm_ug_1_SUITE\""],
	  ?SEC_LDN++",CertM=1"),

    %% startOffline enrollment when the container file is in the PKCS#12 format
    ct:pal("startOfflineEnrollment when the container file is in the PKCS#12 format."),
    ct:pal("Action installCredentialFromUri PKCS#12."),
    
    Pkcs12Uri = sftp_uri("/backup/sftp/etxasta/test_cert/client_syslog.p12"),
    Pkcs12UriPwd = ?SftpPassword, 
    Pkcs12CredPwd = "test",
    Pkcs12Fingerprint = "34:57:11:ed:ae:8e:19:22:4d:d0:e5:8e:83:57:1f:73:f6:97:e0:c6",

    {ok, _} = post_ug_lib:installcredentialfromuri_from_cli("installCredentialFromUri", 
							    Pkcs12Uri, 
							    Pkcs12UriPwd, 
							    Pkcs12CredPwd,
							    Pkcs12Fingerprint, 
							    ?SEC_LDN++",CertM=1"++",NodeCredential=2"),

    
    ok = post_ug_lib:check_progress_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
						  ?SEC_LDN++",CertM=1"++",NodeCredential=2", 40), %%20 orginal


    %% Configure NodeCredential for Offline enrollment CSR-based
    ct:pal("Configure NodeCredential 3, Offline enrollment CSR-based"),
    {ok, _} = 
    	post_ug_lib:create_nodecredential_mo_from_cli(
	  "NodeCredential=3",
	  ["enrollmentAuthority="
	   "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,EnrollmentAuthority=1",
	   "enrollmentServerGroup="
	   "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,EnrollmentServerGroup=1",
	   "enrollmentTimer=5",
	   "expiryAlarmThreshold=50",
	   "keyInfo=RSA_2048",
	   "subjectName=\"C=SE,O=Ericsson,CN=swm_ug_1_SUITE.ericsson.com\"",
	   "userLabel=\"Created by swm_ug_1_SUITE\""],
	  ?SEC_LDN++",CertM=1"),

    %% Starts a manual offlineCsrEnrollment procedure. 
    ct:pal("startOfflineEnrollment, a CSR-based offline enrollment"),
    ct:pal("Action startOfflineCsrEnrollment."),


    CsrPrivDir = proplists:get_value(priv_dir, Config),
    os:cmd(["chmod a+rwx ", CsrPrivDir]),
    CsrUriTest = sftp_uri(CsrPrivDir) ++ "test.csr",
    CsrUriPwd = ?SftpPassword, 

    {ok, _} = post_ug_lib:startofflineenrollment_from_cli("startOfflineCsrEnrollment", 
							  CsrUriTest, 
							  CsrUriPwd, 
							  ?SEC_LDN++",CertM=1"++",NodeCredential=3"),
    
    
    ok = post_ug_lib:check_progress_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
    						  ?SEC_LDN++",CertM=1"++",NodeCredential=3", 40), %%20 orginal


    ct:pal("Action installCredentialFromUri -- from CSR-based offline enrollment."),
    CsrUri = sftp_uri("/backup/sftp/etxasta/test_cert/signed_cert.pem"),
    CsrCredPwd = "true", %% FIXME should be NULL
    %%CsrFingerPrint ="7E:17:89:C7:9C:AD:C2:BC:04:68:AE:70:55:B3:21:71:68:9E:08:BF",
    CsrFingerPrint = "ef:10:58:06:25:59:4a:53:7c:12:4c:e6:21:13:ce:82:88:1b:84:19",
    {ok, _} = post_ug_lib:installcredentialfromuri_from_cli("installCredentialFromUri", 
							    CsrUri, 
							    CsrUriPwd, 
							    CsrCredPwd,
							    CsrFingerPrint, 
							    ?SEC_LDN++",CertM=1"++",NodeCredential=3"),
    
    


    %% NOTE! The progress returns:  result=FAILURE 
    %%                              resultInfo="No match between certificate and private key"
    %%                              state=FINISHED

    %% ok = post_ug_lib:check_progress_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
    %% 						  ?SEC_LDN++",CertM=1"++",NodeCredential=3", 40), %%20 orginal

  
    %% Install Trusted Certificate
    ct:pal("Install TrustedCertificate"),
    

    DataDir = ?CERT_PATH,
    PrivDir = proplists:get_value(priv_dir, Config),
    File = proplists:get_value(file, Config, "test_tc.pem"),
    Path =  filename:join(DataDir, File),
    SftpPath = filename:join(PrivDir, File),
    {ok, _} = file:copy(Path, SftpPath),

    [_, Fingerprint] = 
	string:tokens(
	  cmd(["openssl x509 -fingerprint -noout -in ", Path])--"\n", "="),
    
    Uri = sftp_uri(SftpPath),
    
    {ok, _} = post_ug_lib:install_trustcertificate_mo_from_cli("installTrustedCertFromUri", 
    						     Uri, 
    						     ?SftpPassword, 
    						     Fingerprint, 
    						     ?SEC_LDN++",CertM=1"),
    
    ok = post_ug_lib:check_progress_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
						  ?SEC_LDN++",CertM=1", 40), %%20 orginal
    
    %% Enable Trusted Certificate
    ct:pal("Enable Trusted Certificate"),
    {ok, _} = post_ug_lib:change_attribute_from_cli("managedState=ENABLED", 
						    ?SEC_LDN++",CertM=1"++",TrustedCertificate=2"),

    
    %% Configure TrustCategory
    ct:pal("Configure TrustCategory"),
    {ok, _} = 
    	post_ug_lib:create_trustcategory_mo_from_cli(
	  "TrustCategory=1",
	  ["trustedCertificates="
	   "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustedCertificate=2",
	   "userLabel=\"Created by swm_ug_1_SUITE\""],
	  ?SEC_LDN++",CertM=1"),
    
    
    %% Configure Tls
    ct:pal("Configure Netconf over TLS"),
    {ok, _} = post_ug_lib:change_attributes_from_cli(
		["administrativeState=UNLOCKED",
		 "trustCategory="
		 "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=1",
		 "nodeCredential="
		 "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=1"],             
		?SYS_LDN++",NetconfTls=1"),
 

    %% Configure LDAP for user authentication
    ct:pal("Configure LDAP for user authentication"),

    %% 1. Set attribute administrativeState of MO LdapAuthenticationMethod to UNLOCKED
    {ok, _} = 
	post_ug_lib:change_attribute_from_cli(
	  "administrativeState=UNLOCKED", 
	  ?SEC_LDN++",UserManagement=1"++", LdapAuthenticationMethod=1"),

    %% 2. Configure the following attributes of MO Ldap with values obtained from 
    %%    Node Centralized User Management Integration Guide for OSS-RC:
    %%  - baseDn*, bindDn*, bindPassword*, fallbackLdapIpAddress*. 
    %%    (It is strongly recommended to use redundant LDAP directory servers, 
    %%     However, this attribute can be omitted if no fallback LDAP server exists), 

    {ok, _} = 
	post_ug_lib:change_attributes_from_cli(
	  [%%"baseDn=\"ou=people,dc=mordor,dc=invalid\"", 
	   %%"bindDn=\"cn=king,dc=mordor,dc=invalid\"", 
	   %%"bindPassword=\"1:7TcvZCTcqkKUI6RNL3IKSlMB/kas\"",
	   "fallbackLdapIpAddress=10.68.200.12" 
	   %%"ldapIpAddress=10.68.200.11"	     
	  ],                   
	  ?SEC_LDN++",UserManagement=1"++", LdapAuthenticationMethod=1"++", Ldap=1"),
    
    %% 3. Configure the TLS support in the following attributes of MO Ldap:
    %% -useTls = TRUE. (If set to FALSE, TLS is not used and password information 
    %%  is not encrypted. It is not recommended to send information
    %%  that is not encrypted.
    %% -tlsMode =  LDAPS. It is possible to use STARTTLS, 
    %%  but for OSS integration LDAPS must be used.
    %% -useTlsFallback = true
    %% -nodeCredential - nodeCredential MO.
    %% -trustCategory - trustCatagory MO.

    {ok, _} = 
	post_ug_lib:change_attributes_from_cli(
	  ["useTls=false", 
	   %%"useTls=true", 
	   "tlsMode=LDAPS", 
	   %%"useTlsFallback=true", <deprecated>
	   "fallbackLdapIpAddress=10.68.200.12",
	   "trustCategory=ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=1",
	   "nodeCredential=ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=1"   
	   %%"tlsCaCertificate=", <deprecated> ?
	   %%"tlsClientCertificate=", <deprecated> ?
	   %%"tlsClientKey=", <deprecated> ?
	  ],                   
	  ?SEC_LDN++",UserManagement=1"++", LdapAuthenticationMethod=1"++", Ldap=1"),

    

    %% Configure LDAP for RBAC
    ct:pal("Configure LDAP for RBAC"),

    %% 1. Configure the following attributes in MO Ldap 
    %% (not for secure board, can not find the rollAuthentication then keep profileFilter=POSIX_GROUPS)
    %%  -profileFilter - ERICSSON_FILTER

    case check_if_vc_board() of
	"yes"->
	    {ok, _} = 
		post_ug_lib:change_attribute_from_cli(
		  "profileFilter=POSIX_GROUPS",                   
		  ?SEC_LDN++",UserManagement=1"++", LdapAuthenticationMethod=1"++", Ldap=1");
    	_ -> 
	    {ok, _} = 
		post_ug_lib:change_attribute_from_cli(
		  "profileFilter=ERICSSON_FILTER",                   
		  ?SEC_LDN++",UserManagement=1"++", LdapAuthenticationMethod=1"++", Ldap=1")
    end,	    

    %% 2. Configure attribute roleAliasesBaseDn of MO EricssonFilter. The
    %%    attribute values are described in 
    %%    Node Centralized User Management Integration Guide for OSS-RC.
    {ok, _} = 
	post_ug_lib:change_attribute_from_cli(
	  "roleAliasesBaseDn=\"dc=example,dc=com\"", 
	  ?SEC_LDN++",UserManagement=1"++", LdapAuthenticationMethod=1"++", Ldap=1"++", EricssonFilter=1"),

    
    %% Configure LDAP for TBAC
    ct:pal("Configure LDAP for TBAC"),
    
    %% 1. Set attribute -targetBasedAccessControl of MO EricssonFilter to
    %%    UNLOCKED.
    {ok, _} = post_ug_lib:change_attribute_from_cli(
		"targetBasedAccessControl=LOCKED", 
		%%"targetBasedAccessControl=UNLOCKED", 
		?SEC_LDN++",UserManagement=1"++", LdapAuthenticationMethod=1"++", Ldap=1"++", EricssonFilter=1"),
    

    %% 2. Set one or more target types for TBAC 
    %% - Configure the attribute -targetType of MO UserManagement. The attribute targetType can
    %% contain several values. As a minimum, two strings must be configured:
    %% • The name of the Managed Element, which is the string of the attribute
    %% networkManagedElementId in MO ManagedElement.
    %% • The TCU string, if the Managed Element is a TCU.
    %% • Any other target string corresponding to LDAP directory server
    %% configuration.
    %% For information on ports and protocols, see Node Hardening Guidelines

    {ok, _} = post_ug_lib:change_attributes_from_cli(["targetType=[\"ki.sw.ericsson.se\"]", 
						      "userLabel=\"Created by swm_ug_1_SUITE\""],
						     ?SEC_LDN++",UserManagement=1"),
    

    %% Configure User Management
    ct:pal("Configure User Management"),
    {ok, _} = 
	post_ug_lib:change_attribute_from_cli(
	  "administrativeState=UNLOCKED", 
	  ?SEC_LDN++", UserManagement=1"++", LocalAuthorizationMethod=1"),
    
    ct:pal("Create Custom Rule"),
    %% 1.Create an instance of MO CustomRule under LocalAuthorizationMethod.
    %% 2.Specify the target MO or MO Class that the rule applies to and 
    %%   set attribute ruleData according to the syntax description in the MOM.
    %% 3.Set the access right that a role mapped to this custom rule has on a 
    %%   target object specified in step 2, 
    %%   and set attribute permission of the respective MO instance CustomRule 
    %%   according to the syntax description in the MOM.
    %% 4.Optionally, set the rule name in attribute ruleName of the respective 
    %%   MO instance CustomRule.
    %% 5.Optionally, describe the custom rule policy in a user-friendly manner in 
    %%   attribute userLabel of the respective MO instance CustomRule.
    %%   As a result, a new custom rule is created and can be assigned to custom roles.

    {ok, _} = post_ug_lib:create_customrule_mo_from_cli(
		"CustomRule=UGconfigtest", 
		["ruleData=\"ManagedElement,*\"", 
		 "permission=RX",
		 "ruleName=UGconfigtest",
		 "userLabel=\"Test rule for UG CS configurtion test\""
		],
		?SEC_LDN++", UserManagement=1"++", LocalAuthorizationMethod=1"),


    ct:pal("Create Custom Role"),
    %% 1.Create an instance of MO CustomRole under LocalAuthorizationMethod.
    %% 2.Set the custom role name in attribute roleName of the respective MO instance CustomRole.
    %% 3.Assign at least one custom rule to the newly created custom role by 
    %%   adding at least one MO instance CustomRule in attribute rules of the respective 
    %%   MO instance CustomRole. 
    %% 4.Optionally, describe the custom rule policy in a user-friendly manner in attribute 
    %%   userLabel of the respective MO instance CustomRole.
    %%   As a result, the custom role is created and can be assigned to user accounts.

    {ok, _} = 
    	post_ug_lib:create_customrole_mo_from_cli(
    	  "CustomRole=1", 
    	  ["roleName=UGconfigtestUser",
    	   "rules="
    	   "ManagedElement=1,SystemFunctions=1,SecM=1,UserManagement=1,LocalAuthorizationMethod=1,CustomRule=UGconfigtest",
    	   "userLabel=\"Test role for UG CS configurtion test\""
    	  ],
    	  ?SEC_LDN++", UserManagement=1"++", LocalAuthorizationMethod=1"),
    
    ct:pal("Add a Maintenance User for TLS"),
    %%Testa manuellt?

    ct:pal("Add a Maintenance User for SSH"),
    %%Testa manuellt?
    

    %%test_server:break("Custom RoleName"),

    ct:pal("Configure Simple Network Management Protocol"),
    %% SNMP is used for alarm handling. 
    %% SNMPv2 is recommended to be used towards OSS-RC.
    %% SNMPv3 can be configured as an alternative.
    %% The following must be configured for MO Snmp:
    %% -agentAddress
    %% -administrativeState must be UNLOCKED for alarms to be sent and to enable the SNMP access. 
    {ok, _} = post_ug_lib:change_attributes_from_cli(["administrativeState=UNLOCKED",
    						      "agentAddress[@1]",
    						      "host=10.86.148.116",
    						      "port=6161"
    						     ],                   
    						     ?SYS_LDN++", Snmp=1"),
    

    %% To configure SNMPv2, set the following attributes of MO SnmpTargetV2C:
    %% -community 
    %% -address
    {ok, _} = 
	post_ug_lib:change_attributes_from_cli(["community=public",
						"address=147.214.13.193"
					       ],                   
					       ?SYS_LDN++", Snmp=1"++", SnmpTargetV2C=1"),

    %% To configure SNMPv3, set the following attributes of MO SnmpTargetV3:
    %% -user 
    %% -address
    %% -privKey
    %% -authKey  
    {ok, _} = 
	post_ug_lib:create_snmptargetv3_mo_from_cli("SnmpTargetV3=1", 
						    ["user=\"UG testuser\"",
						     "address=10.86.148.116",
						     "privKey=\"1:7X5JtQp002fmVofZIU/ai3yVW5s=\"",
						     "authKey=\"1:Ovwm5DYH4c7eMDza201whYDDpEY=\""
						    ],
						    ?SYS_LDN++", Snmp=1"),
       
    
    %% Following attributes must be useTls=false, targetBasedAccessControl=LOCKED", 
    %% to get the suite to work.

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Log Management (ManagedElement=1,SystemFunctions=1,LogM=1)   
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %% Configure Log
    ct:pal("Configure Log"),
    {ok, _} = post_ug_lib:change_attributes_from_cli(
		["trustCategory="
		 "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=1",
		 "nodeCredential="
		 "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=1"],            
		?LOG_LDN),

    %% Configure and export SwmLog
    ct:pal("Configure and Export SwmLog"),
    {ok, _} = post_ug_lib:change_attribute_from_cli("severityFilter=[CRITICAL,WARNING]", 
						    ?LOG_LDN++", Log=SwmLog"),
    {ok, _} = post_ug_lib:change_attribute_from_cli("severityFilter=INFO", 
						    ?LOG_LDN++", Log=SwmLog"),

    %% Export SwmLog
    SwmLogPath = ?config(priv_dir,Config),
    ct:pal("SwmLogPath: ~p~n", [SwmLogPath]),
    os:cmd("chmod 777 "++SwmLogPath), % else permission.
    
    SwmUri = sftp_uri(SwmLogPath),  
    {ok, _} = post_ug_lib:fetch_log("export", SwmUri, ?SftpPassword, ?LOG_LDN++", Log=SwmLog"),

    {ok, _SwmLogData} = 
	post_ug_lib:check_log_progress_data_from_cli(
	  ["result=SUCCESS", "state=FINISHED"], ?LOG_LDN++", Log=SwmLog", 20),
    
    %% Configure Push Log
    ct:pal("Configure Push Log"),
    
    [{host, SysLogUdpHost},{username, _Username},{password, _Password}] 
	= ct:get_config(syslog_udp_server),

    {ok, _} = 
    	post_ug_lib:create_pushlog_mo_from_cli("LogPushTransfer=1",
					       ["transferType=STREAM",
						"uri=syslog://"++ SysLogUdpHost
						%%"password=\"Created by swm_ug_1_SUITE\""
					       ],
					       ?LOG_LDN++", Log=SecurityLog"),
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Export Availibility Log
    %%%%%%%%%%%%%%%%%%%%%%%%%%

    ct:pal("Export Availibility Log"),
    AvliLogPath=?config(priv_dir,Config),
    ct:pal("AvliLogPath: ~p~n", [AvliLogPath]),
    os:cmd("chmod 777 "++AvliLogPath), % else permission.
    
    AvliUri = sftp_uri(AvliLogPath),    
    {ok, _} = post_ug_lib:fetch_avli("exportAvailabilityLog", AvliUri, ?SftpPassword, ?LOG_LDN),

    {ok, AvliLogData} = 
	post_ug_lib:check_log_progress_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
						     ?LOG_LDN, 20),
    post_ug_lib:unpack_avli_log_data_from_cli(AvliLogData, AvliLogPath, AvliLogPath, 
					      ?SftpHost, ?SftpUser, ?SftpPassword),

    %%%%%%%%%%%%%
    %% Export ESI
    %%%%%%%%%%%%%

    ct:pal("Export ESI"),
    EsiLogPath=?config(priv_dir,Config),
    ct:pal("EsiLogPath: ~p~n", [EsiLogPath]),
    os:cmd("chmod 777 "++EsiLogPath), % else permission.
    
    EsiUri = sftp_uri(EsiLogPath),    
    {ok, _} = post_ug_lib:fetch_esi("exportEsi", EsiUri, ?SftpPassword, ?LOG_LDN),

    {ok, EsiLogData} = 
	post_ug_lib:check_log_progress_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
						     ?LOG_LDN, 80),   

    post_ug_lib:unpack_esi_log_data_from_cli(EsiLogData, EsiLogPath, "resultInfo=", 
					     ?SftpHost, ?SftpUser, ?SftpPassword),

    ok = rct_cli:disconnect(?CLI_Session),  
    Config.

%%%--------------------------------------------------------------------
%%% @doc
%%% Check that CS MW configuration are the same as before upgrade via CLI. <br/>
%%% @spec after_ug(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
after_ug(Config) ->
    ct:pal("sleep 2min to make it more robust"),
    timer:sleep(120000),
    %% Get managedElementId
    %%MeId = proplists:get_value(meId, Config), 
    MeId = swm_test_lib:get_me_id(?NC_Session),
    
    
    ok = rct_cli:connect(?CLI_Session),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% License Management (ManagedElement=1,SystemFunctions=1,Lm=1)   
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    %% Check License Key File Fault Alarm (Critical) NOT exist
    wait_for_exp_alarm_to_cease(?NC_Session, MeId, ?LiKeyFault),
    
    %% Check FeatureState still ACTVATED
    ct:pal("Check featureState ACTIVATED after Upgrade"),

    ok = post_ug_lib:check_data_from_cli(["featureState=ACTIVATED", 
   					  "licenseState=ENABLED", 
   					  "serviceState=OPERABLE"], 
   					 ?LM_LDN ++ ",FeatureState=CXC4010320", 3),
    
    ok = post_ug_lib:check_data_from_cli(["featureState=ACTIVATED", 
   					  "licenseState=ENABLED", 
   					  "serviceState=OPERABLE"], 
   					 ?LM_LDN ++ ",FeatureState=CXC4010511", 3),
    
    %% Check licenseState is enabled
    ct:pal("Check licenseState ENABLED after Upgrade"),
    ok = post_ug_lib:check_data_from_cli(["licenseState=ENABLED"], 
   					?LM_LDN ++ ",CapacityState=CXC4011956", 3),
    
    %% Check Emergacy unlock Activated
    ct:pal("Check Emergacy unlock Attributes and Alarm after Upgrade"),
    ok = post_ug_lib:check_data_from_cli(["activationState=ACTIVATED"], ?EU_LDN, 3),
    ok = post_ug_lib:check_data_from_cli(["activationsLeft=1"], ?EU_LDN, 1),
    
    ct:pal("Check Lm stat EMERGENCY_UNLOCK after Upgrade"),
    ok = post_ug_lib:check_data_from_cli(["lmState=EMERGENCY_UNLOCK"], ?LM_LDN, 3),	    

    %% Check Emergency Unlock Reset Key Required Alarm (Warning) exist
    wait_for_exp_alarm_to_exist(?NC_Session, MeId, ?EmUnlockResetKeyRequired),
    {_, ok} = post_ug_lib:find_mo_from_cli("FmAlarm=", ?FM_LDN, 
   					  ["minorType=9175048","activeSeverity=WARNING"], 
   					  3, ok),

    %% Check attribute fingerprint NOT possible to change
    ct:pal("Try to change fingerprint after Upgarde"), 
    post_ug_lib:change_fingerprint_attribute_from_cli("fingerprint=Upgrade", ?LM_LDN),
    %%nok = post_ug_lib:check_data_from_cli(["fingerprint=\"Upgrade"], ?LM_LDN, 0),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Fault Management (ManagedElement=1,SystemFunctions=1,Fm=1)   
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %% Check Attribute, heartbeatInterval and configuredSeverity
    ct:pal("Check heartbeatInterval and configuredSeverity"),
    ok = post_ug_lib:check_data_from_cli(["heartbeatInterval=30"], ?FM_LDN, 3),
    ok = post_ug_lib:check_data_from_cli(
	   ["configuredSeverity=MINOR"], 
	   ?FM_LDN ++ ",FmAlarmModel=1,FmAlarmType=EnclosureDoorOpen",
	   3),
    
      
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Performance Management (ManagedElement=1,SystemFunctions=1,Pm=1)   
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %%Find user-defined PM jobs
    
    ct:pal("Find PmJob=1"),
    {_, ok} = post_ug_lib:find_pmjob_from_cli(
		?PM_LDN, "PmJob=1", 
		["requestedJobState=ACTIVE"], 
		"MeasurementReader=1", 
		"measurementSpecification",
		"groupRef="
		"ManagedElement=1,SystemFunctions=1,Pm=1,PmGroup=Group1", 3),
    
    ct:pal("Find PmJob=2"),
    
    {_, ok} = post_ug_lib:find_pmjob_from_cli(
		?PM_LDN, "PmJob=2",
		["jobPriority=LOW", 
		 "jobType=THRESHOLDJOB",
		 "requestedJobState=STOPPED"],
		"MeasurementReader=2",
		"measurementSpecification", 
		"groupRef=ManagedElement=1,SystemFunctions=1,Pm=1,PmGroup=Group2", 3),
    
    ct:pal("Find PmJob=3"),
    {_, ok} = post_ug_lib:find_pmjob_from_cli(
		?PM_LDN, "PmJob=3",
		["requestedJobState=ACTIVE"],
		"MeasurementReader=1",
		"measurementSpecification", 
		"groupRef=ManagedElement=1,SystemFunctions=1,Pm=1,PmGroup=ROPGroup2", 3),
    
    ct:pal("Find PmJob=4"),
    {_, ok} = 
	post_ug_lib:find_pmjob_from_cli(
	  ?PM_LDN, "PmJob=4",
	  ["compressionType=GZIP","jobPriority=HIGH",
	   "requestedJobState=ACTIVE"],
	  "MeasurementReader=2",
	  "measurementSpecification", 
	  "groupRef=ManagedElement=1,SystemFunctions=1,Pm=1,PmGroup=ROPGroupMult", 3),
    
    
    ct:pal("Find EventJob=6"),

    RatType = get_rat_type(),
   
    case RatType of
    	grat ->
	    {_, ok} = 
		post_ug_lib:find_pmevent_from_cli(
		  ?PMEVENT_LDN ++ ",EventProducer=Grat",
		  "EventJob=6",
		  ["fileOutputEnabled=true","fileCompressionType=GZIP",
		   "reportingPeriod=FIFTEEN_MIN", "streamOutputEnabled=true",
		   "streamCompressionType=GZIP","streamDestinationIpAddress=\"123.1.2.3\"",
		   "streamDestinationPort=2205"],
		  ["eventGroupRef",
		   "ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp1",
		   "ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp2"],
		  ["eventTypeRef",
		   "ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp1,EventType=EvType1",
		   "ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp2,EventType=EvType2"],
		  ["eventFilter[@1]","filterName=\"SingleSelect\"","filterValue=\"ue\""],
		  3);
	wrat ->
	    %% NOT IMPLEMENTED YET!!

	    %% {_, ok} = 
	    %% 	post_ug_lib:find_pmevent_from_cli(
	    %% 	  ?PMEVENT_LDN ++ ",EventProducer=Wrat",
	    %% 	  "EventJob=6",
	    %% 	  ["fileOutputEnabled=true","fileCompressionType=GZIP",
	    %% 	   "reportingPeriod=FIFTEEN_MIN", "streamOutputEnabled=true",
	    %% 	   "streamCompressionType=GZIP","streamDestinationIpAddress=\"123.1.2.3\"",
	    %% 	   "streamDestinationPort=2205"],
	    %% 	  ["eventGroupRef",
	    %% 	   "ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp1",
	    %% 	   "ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp2"],
	    %% 	  ["eventTypeRef",
	    %% 	   "ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp1,EventType=EvType1",
	    %% 	   "ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp2,EventType=EvType2"],
	    %% 	  ["eventFilter[@1]","filterName=\"SingleSelect\"","filterValue=\"ue\""],
	    %% 	  3);
	     continue;
    	lrat ->
	    {_, ok} = 
		post_ug_lib:find_pmevent_from_cli(
		  ?PMEVENT_LDN ++ ",EventProducer=Lrat",
		  "EventJob=6",
		  ["fileOutputEnabled=true","fileCompressionType=GZIP",
		   "reportingPeriod=FIFTEEN_MIN", "streamOutputEnabled=true",
		   "streamCompressionType=GZIP","streamDestinationIpAddress=\"123.1.2.3\"",
		   "streamDestinationPort=2205"],
		  ["eventGroupRef",
		   "ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp1",
		   "ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp2"],
		  ["eventTypeRef",
		   "ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp1,EventType=EvType1",
		   "ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp2,EventType=EvType2"],
		  ["eventFilter[@1]","filterName=\"SingleSelect\"","filterValue=\"ue\""],
		  3);
    	tcu ->
	    %% NOT VALID!!
	    continue;
	
    	_Else ->
	    {_, ok} = 
		post_ug_lib:find_pmevent_from_cli(
		  ?PMEVENT_LDN ++ ",EventProducer=first",
		  "EventJob=6",
		  ["fileOutputEnabled=true","fileCompressionType=GZIP",
		   "reportingPeriod=FIFTEEN_MIN", "streamOutputEnabled=true",
		   "streamCompressionType=GZIP","streamDestinationIpAddress=\"123.1.2.3\"",
		   "streamDestinationPort=2205"],
		  ["eventGroupRef",
		   "ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp1",
		   "ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp2"],
		  ["eventTypeRef",
		   "ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp1,EventType=EvType1",
		   "ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp2,EventType=EvType2"],
		  ["eventFilter[@1]","filterName=\"SingleSelect\"","filterValue=\"ue\""],
		  3)
    end,
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Software Management (ManagedElement=1,SystemFunctions=1,SwM=1)  
    %% Backup and restore Management (ManagedElement=1,SystemFunctions=1,BrM=1) 
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %%  Check fallbackTimer
    ct:pal("Check fallbackTimer"),
    %% HV14974 - set fallbackTimer to 1800 since it was set to lower value (1000)
    ok = post_ug_lib:check_data_from_cli(["fallbackTimer=1000"], ?SWM_LDN, 3),
    	  
    %% Backup and Restore
    ct:pal("Check exportPackageLabelPrefix"),
    ok = post_ug_lib:check_data_from_cli(["exportPackageLabelPrefix=\"upgrade_config_test\""], 
					 ?BRM_LDN, 3),
    
    %% Housekeeping + autoDelete
    ct:pal("Check housekeeping and autoDelete"),
    ok = 
	post_ug_lib:check_data_from_cli(["maxStoredManualBackups=18",
					 "autoDelete=DISABLED"], 
					?BRM_LDN++",BrmBackupManager=1,BrmBackupHousekeeping=1", 3),
  
    %% Check Configure Automatic Backup configuration
    
    %% Configure Backup Scheduler"
    ct:pal("Check Backup Scheduler"),
    ok = post_ug_lib:check_data_from_cli(["maxStoredScheduledBackups=4",
					  "scheduledBackupName=\"BACKUP_CONFIGURATION_TEST\""], 
    					 ?BRM_LDN++",BrmBackupManager=1,BrmBackupScheduler=1", 3),
    

    ct:pal("Check Single Scheduled Backup Event"),
    ok = post_ug_lib:find_single_scheduler_backup_event(
	   ?BRM_LDN++",BrmBackupManager=1,BrmBackupScheduler=1",
	   "BrmSingleEvent=1", 
	   ["scheduledTime=\"2030-04-05T02:00:00\""], 3),
    
	
    ct:pal("Check Periodic Scheduled Backup Event Based on Calendar"),
    ok = post_ug_lib:find_periodic_scheduler_backup_event_calender(
	   ?BRM_LDN++",BrmBackupManager=1,BrmBackupScheduler=1",
	   "BrmCalendarBasedPeriodicEvent=1",
	   ["dayOfMonth=17","dayOfWeek=THURSDAY",
	    "dayOfWeekOccurrence=THIRD","month=12",
	    "startTime=\"2030-12-17T02:00:00\"",
	    "stopTime=\"2030-12-17T04:00:00\"",
	    "time=\"02:00:00\""], 
	   3),
    

    ct:pal("Check Periodic Scheduled Backup Event"),
    ok = post_ug_lib:find_periodic_scheduler_backup_event(
	   ?BRM_LDN++",BrmBackupManager=1,BrmBackupScheduler=1",
	   "BrmPeriodicEvent=1",
	   ["days=20","hours=12","minutes=10",
	    "startTime=\"2030-08-17T04:00:00\"",
	    "stopTime=\"2030-09-17T04:00:00\"","weeks=4"], 
	   3),


    %% Check Automatic Export of Scheduled Backup 
    ct:pal("Check Automatic Export of Scheduled Backup"),
    ok = post_ug_lib:check_data_from_cli(["autoExportPassword=\"1:db/ejy4EkZ3jHzU1PuEXktY1504=\"",
					  "autoExportUri=\"http://www.testweb.se\"",
					  "autoExport=ENABLED"],                    
					 ?BRM_LDN++",BrmBackupManager=1,BrmBackupScheduler=1", 
					 3),

    
    ct:pal("Check Suspend Backup Scheduler"),
    ok = post_ug_lib:check_data_from_cli(["adminState=LOCKED"], 
    					 ?BRM_LDN++",BrmBackupManager=1,BrmBackupScheduler=1", 
					 3),
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Security Management (ManagedElement=1,SystemFunctions=1,SecM=1)   
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ct:pal("Check Security Management"),
    ct:pal("Check O&M Access Point"),
    
    case RatType of
    	grat ->
    	    %% configure_node_ipv4address_suppot_via_cli(),
	    
    	    %% {ok, _} = post_ug_lib:change_attributes_from_cli(
    	    %% 		["ipv4address=ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=1,AddressIPv4=1", 
    	    %% 		 "dscp=40"
    	    %% 		 %% "netconfPort=2222",
    	    %% 		 %% "sshPort=3333"
    	    %% 		],                   
    	    %% 		?SYS_LDN++",OamAccessPoint=1");
	    continue;
    	wrat ->
    	    %% configure_node_ipv4address_suppot_via_cli(),

    	    %% {ok, _} = post_ug_lib:change_attributes_from_cli(
    	    %% 		["ipv4address=ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=1,AddressIPv4=1", 
    	    %% 		 "dscp=40"
    	    %% 		 %% "netconfPort=2222",
    	    %% 		 %% "sshPort=3333"
    	    %% 		],                   
    	    %% 		?SYS_LDN++",OamAccessPoint=1");  
	    continue;
    	lrat ->
    	    %% configure_node_ipv4address_suppot_via_cli(),

    	    %% {ok, _} = post_ug_lib:change_attributes_from_cli(
    	    %% 		["ipv4address=ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=1,AddressIPv4=1",
    	    %% 		 "dscp=40"
    	    %% 		 %% "netconfPort=2222",
    	    %% 		 %% "sshPort=3333"
    	    %% 		],                   
    	    %% 		?SYS_LDN++",OamAccessPoint=1");  
	    continue;
    	tcu ->
    	    %% configure_node_ipv4address_suppot_via_cli(),

    	    %% {ok, _} = post_ug_lib:change_attributes_from_cli(
    	    %% 		["ipv4address=ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=1,AddressIPv4=1", 
    	    %% 		 "dscp=40"
    	    %% 		 %% "netconfPort=2222",
    	    %% 		 %% "sshPort=3333"
    	    %% 		],                   
    	    %% 		?SYS_LDN++",OamAccessPoint=1");  
	    continue;
	
    	_Else1 ->
    	    ok = post_ug_lib:check_data_from_cli(
		   ["ipv4address=\"ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=1,AddressIPv4=1\"", 
		    "dscp=40"], 
		   ?SYS_LDN++",OamAccessPoint=1",
		   3)
    end,
    
    %% Check CertM
    ct:pal("Check CertM, userLabel"),
    ok = post_ug_lib:check_data_from_cli(["userLabel=\"Created by swm_ug_1_SUITE\""],    
					 ?SEC_LDN++",CertM=1",
					 3),
    
    %% Check  CertMCapabilities
    ct:pal("Check CertMCapabilities MO"),
    ok = post_ug_lib:check_sec_mo_and_data_from_cli(?SEC_LDN++",CertM=1",
						    "CertMCapabilities=1",
						    [],
						    3),

    %% Check EnrollmentAuthority 

    EADataDir = ?CERT_PATH,
    EAFile = proplists:get_value(file, Config, "etxasta.pem"),
    EAPath =  filename:join(EADataDir, EAFile),

    [_, FP] = string:tokens(cmd(["openssl x509 -fingerprint -noout -in ", EAPath]), "="),

    EnAFingerprint = string:strip(FP, both, $\n),
    EnrollmentCaFingerprint  = lists:concat(["enrollmentCaFingerprint=\"", EnAFingerprint, "\""]),

    ct:pal("Check EnrollmentAuthority MO"),
    ok = post_ug_lib:check_sec_mo_and_data_from_cli(
	   ?SEC_LDN++",CertM=1",
	   "EnrollmentAuthority=1",
	   ["authorityType=CERTIFICATION_AUTHORITY",
	    "enrollmentAuthorityName=\"C=SE,O=Ericsson,CN=etxmlar\"",
        %"enrollmentCaCertificate=\"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustedCertificate=2\"",
	    EnrollmentCaFingerprint,
	    "userLabel=\"Created by swm_ug_1_SUITE\""],
	   3),


    %% Check EnrollmentServerGrpup
    ct:pal("Check EnrollmentServerGroup MO"),
    ok = post_ug_lib:check_sec_mo_and_data_from_cli(?SEC_LDN++",CertM=1",
						    "EnrollmentServerGroup=1",
						    ["userLabel=\"Created by swm_ug_1_SUITE\""],
						    3),

    %% Check EnrollmentServer

    [{host, Cmpv2Server}] = ct:get_config(cmpv2_server),

    Cmpv2Host = "http://"++Cmpv2Server++":8180/",

    ESUri = lists:concat(["uri=\"", Cmpv2Host, "\""]),

    ct:pal("Check EnrollmentServer MO"),
    ok = post_ug_lib:check_sec_mo_and_data_from_cli(
    	   ?SEC_LDN++",CertM=1"++",EnrollmentServerGroup=1",
    	   "EnrollmentServer=1",
    	   ["enrollmentAuthority=\"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,EnrollmentAuthority=1\"",
    	    "protocol=CMP",
    	    ESUri,
    	    "userLabel=\"Created by swm_ug_1_SUITE\""],
     	   3),

    ct:pal("Check EnrollmentServer MO"),
    ok = post_ug_lib:check_sec_mo_and_data_from_cli(
    	   ?SEC_LDN++",CertM=1"++",EnrollmentServerGroup=1",
    	   "EnrollmentServer=1",
    	   ["enrollmentAuthority=\"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,EnrollmentAuthority=1\"",
    	    "protocol=CMP",
    	    ESUri,
    	    "userLabel=\"Created by swm_ug_1_SUITE\""],
    	   3),
    

    %% Check NodeCredential 
    ct:pal("Check NodeCredential 1"),
    ok = post_ug_lib:check_sec_mo_and_data_from_cli(
	   ?SEC_LDN++",CertM=1",
	   "NodeCredential=1",
	   ["enrollmentAuthority=\"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,EnrollmentAuthority=1\"",
	    "enrollmentServerGroup=\"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,EnrollmentServerGroup=1\"",
	    "enrollmentTimer=5",
	    "expiryAlarmThreshold=50",
	    "keyInfo=RSA_2048",
	    "renewalMode=AUTOMATIC",
	    "subjectName=\"C=SE,O=Ericsson,CN=etxmlar1\"",
	    "userLabel=\"Created by swm_ug_1_SUITE\""],
	   3),
   

    ct:pal("Check NodeCredential 2"),
    ok = post_ug_lib:check_sec_mo_and_data_from_cli(
	   ?SEC_LDN++",CertM=1",
	   "NodeCredential=2",
	   ["certificateState=VALID",
	    "enrollmentAuthority=\"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,EnrollmentAuthority=1\"",
	    "enrollmentServerGroup=\"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,EnrollmentServerGroup=1\"",
	    "enrollmentTimer=5",
	    "expiryAlarmThreshold=50",
	    "keyInfo=RSA_2048",
	    "renewalMode=MANUAL",
	    "subjectName=\"C=SE,O=Ericsson,CN=swm_ug_1_SUITE_pkcs12.ericsson.com\"",
	    "userLabel=\"Created by swm_ug_1_SUITE\""],
	   3),
    
    ct:pal("Check NodeCredential 3"),
    ok = post_ug_lib:check_sec_mo_and_data_from_cli(
	   ?SEC_LDN++",CertM=1",
	   "NodeCredential=3",
	   ["enrollmentAuthority=\"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,EnrollmentAuthority=1\"",
	    "enrollmentServerGroup=\"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,EnrollmentServerGroup=1\"",
	    "enrollmentTimer=5",
	    "expiryAlarmThreshold=50",
	    "keyInfo=RSA_2048",
	    "renewalMode=MANUAL",
	    "subjectName=\"C=SE,O=Ericsson,CN=swm_ug_1_SUITE.ericsson.com\"",
	    "userLabel=\"Created by swm_ug_1_SUITE\""],
	   3),


    ct:pal("Check TrustedCertificate 1"),
    ok = post_ug_lib:check_sec_mo_and_data_from_cli(?SEC_LDN++",CertM=1",
						    "TrustedCertificate=1",
						    ["managedState=ENABLED",
						     "certificateState=VALID"],
						    3),

    ct:pal("Check TrustedCertificate 2"),
    ok = post_ug_lib:check_sec_mo_and_data_from_cli(?SEC_LDN++",CertM=1",
						    "TrustedCertificate=2",
						    ["managedState=ENABLED",
						     "certificateState=VALID"],
						    3),

    %% Check TrustCategory
    ct:pal("Check TrustCategory MO"),
    ok = post_ug_lib:check_sec_mo_object_and_data_from_cli(
	   ?SEC_LDN++",CertM=1",
	   "TrustCategory=1",
	   ["trustedCertificates",
	    "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustedCertificate=2"],
	   ["userLabel=\"Created by swm_ug_1_SUITE\""],
	   3),
    
    %% Check VendorCredential 
    ct:pal("Check VendorCredential MO"),
    ok = post_ug_lib:check_sec_mo_and_data_from_cli(?SEC_LDN++",CertM=1",
						    "VendorCredential=1",
						    ["certificateState=VALID"],
						    3),
    

    %% Check Tls
    ct:pal("Check Netconf over TLS"),
    ok  = post_ug_lib:check_data_from_cli(["administrativeState=UNLOCKED",
					   "trustCategory=\"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=1\"",
					   "nodeCredential=\"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=1\""],                   
					  ?SYS_LDN++",NetconfTls=1",
					  3),


    %% Check LDAP for user authentication
    ct:pal("Check LDAP for user authentication"),

    LdapServerAddress = get_ldap_server_ipaddress(),
    LdapIpAddress = lists:concat(["ldapIpAddress=\"", LdapServerAddress, "\""]),
    ct:pal("LdapIpAddress: ~p ~n", [LdapIpAddress]),

    %%Ex. Attribute = "ldapIpAddress=\"10.68.101.150\"


    ok  = post_ug_lib:check_data_from_cli(
	    ["administrativeState=UNLOCKED"],                   
	    ?SEC_LDN++",UserManagement=1"++", LdapAuthenticationMethod=1",
	    3),
    
    ok  = post_ug_lib:check_data_from_cli(
	    ["baseDn=\"ou=people,dc=mordor,dc=invalid\"", 
	     "bindDn=\"cn=king,dc=mordor,dc=invalid\"", 
	     "bindPassword=\"1:7TcvZCTcqkKUI6RNL3IKSlMB/kas\"",
	     "fallbackLdapIpAddress=\"10.68.200.12\"", 
	     LdapIpAddress
	    ],                                     
	    ?SEC_LDN++",UserManagement=1"++", LdapAuthenticationMethod=1"++", Ldap=1",
	    3),
    

    ok  = post_ug_lib:check_data_from_cli(
	    ["useTls=false", 
	     %%"useTls=true",
	     "tlsMode=LDAPS", 
	     %%"useTlsFallback=true", 
	     "fallbackLdapIpAddress=\"10.68.200.12\"",
	     "trustCategory=\"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=1\"",
	     "nodeCredential=\"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=1\""   
	     %%"tlsCaCertificate=", <deprecated> ?
	     %%"tlsClientCertificate=", <deprecated> ?
	     %%"tlsClientKey=", <deprecated> ?	   
	    ],                   
	    ?SEC_LDN++",UserManagement=1"++", LdapAuthenticationMethod=1"++", Ldap=1",
	    3),
  

    %% Check LDAP for RBAC
    ct:pal("Check LDAP for RBAC"),
    
    case check_if_vc_board() of
	"yes"->
	    ok = post_ug_lib:check_data_from_cli(
		   ["profileFilter=POSIX_GROUPS"],                   
		   ?SEC_LDN++",UserManagement=1"++", LdapAuthenticationMethod=1"++", Ldap=1",
		   3);
	_ ->
	    ok = post_ug_lib:check_data_from_cli(
		   ["profileFilter=ERICSSON_FILTER"],                   
		   ?SEC_LDN++",UserManagement=1"++", LdapAuthenticationMethod=1"++", Ldap=1",
		   3)
    end,	    
    
    ok = post_ug_lib:check_data_from_cli(
	   ["roleAliasesBaseDn=\"dc=example,dc=com\""], 
	   ?SEC_LDN++",UserManagement=1"++", LdapAuthenticationMethod=1"++", Ldap=1"++", EricssonFilter=1",
	   3),
    
    %% Check LDAP for TBAC
    ct:pal("Check LDAP for TBAC"),    
    ok = post_ug_lib:check_data_from_cli(
	   ["targetBasedAccessControl=LOCKED"], 
	   %%"targetBasedAccessControl=UNLOCKED", 
	   ?SEC_LDN++",UserManagement=1"++", LdapAuthenticationMethod=1"++", Ldap=1"++", EricssonFilter=1",
	   3),
    
    ok = post_ug_lib:check_sec_mo_object_and_data_from_cli(
	   ?SEC_LDN++",UserManagement=1",
	   noMo,
	   ["targetType", "ki.sw.ericsson.se"],	  
	   ["userLabel=\"Created by swm_ug_1_SUITE\""], 
	   3),

    %% Check User Management
    ct:pal("Check User Management"),
    ok = post_ug_lib:check_data_from_cli(
	   ["administrativeState=UNLOCKED"], 
	   ?SEC_LDN++", UserManagement=1"++", LocalAuthorizationMethod=1",
	   3),
    
    ct:pal("Check Custom Rule MO"),
    ok = post_ug_lib:check_sec_mo_and_data_from_cli(
	   ?SEC_LDN++", UserManagement=1"++", LocalAuthorizationMethod=1",
	   "CustomRule=UGconfigtest", 
	   ["ruleData=\"ManagedElement,*", 
	    "permission=RX",
	    "ruleName=\"UGconfigtest\"",
	    "userLabel=\"Test rule for UG CS configurtion test\""
	   ],
	   3),


    ct:pal("Check Custom Role MO"),
    ok = post_ug_lib:check_sec_mo_object_and_data_from_cli(
	   ?SEC_LDN++", UserManagement=1"++", LocalAuthorizationMethod=1",
	   "CustomRole=1",
	   ["rules",
	    "ManagedElement=1,SystemFunctions=1,SecM=1,UserManagement=1,LocalAuthorizationMethod=1,CustomRule=UGconfigtest"],
	   ["roleName=\"UGconfigtestUser\"","userLabel=\"Test role for UG CS configurtion test\""],
	   3),
    
    %%ct:pal("Check Maintenance User for TLS"),
    %%Testa manuellt?

    %%ct:pal("Check Maintenance User for SSH"),
    %%Testa manuellt?

    ct:pal("Check Simple Network Management Protocol"),
    ok = post_ug_lib:check_sec_mo_object_and_data_from_cli(?SYS_LDN++", Snmp=1",
							   noMo,
							   ["agentAddress[@1]",
							    "host=10.86.148.116",
							    "port=6161"],
							   ["administrativeState=UNLOCKED"],
							   3),
    
    ok = post_ug_lib:check_data_from_cli(["community=\"public\"",
					  "address=\"147.214.13.193\""],                   
					 ?SYS_LDN++", Snmp=1"++", SnmpTargetV2C=1",
					 3),
    
    
    ok = 
	post_ug_lib:check_sec_mo_and_data_from_cli(?SYS_LDN++", Snmp=1",
						   "SnmpTargetV3=1", 
						   ["user=\"UG testuser\"",
						    "address=\"10.86.148.116\"",
						    "privKey=\"1:7X5JtQp002fmVofZIU/ai3yVW5s=\"",
						    "authKey=\"1:Ovwm5DYH4c7eMDza201whYDDpEY=\""],
						   3),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Log Management (ManagedElement=1,SystemFunctions=1,LogM=1)   
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
    %% Check Log
    ct:pal("Check Log"),
    ok = 
	post_ug_lib:check_data_from_cli(
	  ["trustCategory=\"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=1\"",
	   "nodeCredential=\"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=1\""],
	  ?LOG_LDN,
	  3),
   
    %% Configure and export SwmLog
    ct:pal("Check and Export SwmLog"),
    ok = post_ug_lib:find_swm_log_data(?LOG_LDN++", Log=SwmLog",
				       ["severityFilter","CRITICAL","WARNING","INFO"]),
    
    %% %% Export SwmLog
    SwmLogPath = ?config(priv_dir,Config),
    ct:pal("SwmLogPath: ~p~n", [SwmLogPath]),
    os:cmd("chmod 777 "++SwmLogPath), % else permission.
    
    SwmUri = sftp_uri(SwmLogPath),  

    {ok, _} = post_ug_lib:fetch_log("export", SwmUri, ?SftpPassword, ?LOG_LDN++", Log=SwmLog"),
    {ok, _SwmLogData} = 
	post_ug_lib:check_log_progress_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
						     ?LOG_LDN++", Log=SwmLog", 20),
   
    %% Configure Push Log
    ct:pal("Check Push Log"),
    [{host, SysLogUdpHost},{username, _Username},{password, _Password}] 
	= ct:get_config(syslog_udp_server),
    
    ok = post_ug_lib:check_push_log_mo_and_data_from_cli(?LOG_LDN++", Log=SecurityLog",
							 "LogPushTransfer=1",
							 ["transferType=STREAM",
							  "uri=\"syslog://"++ SysLogUdpHost
							  %%"password=\"Created by swm_ug_1_SUITE\""
							 ],
							 3),
    

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Export Availibility Log after upgrade
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ct:pal("Export and Check Availibility Log"),
    AvliLogPath=?config(priv_dir,Config),
    ct:pal("AvliLogPath: ~p~n", [AvliLogPath]),
    os:cmd("chmod 777 "++AvliLogPath), % else permission.
    
    AvliUri = sftp_uri(AvliLogPath),
    
    {ok, _} = post_ug_lib:fetch_avli("exportAvailabilityLog", AvliUri, ?SftpPassword, ?LOG_LDN),
    {ok, AvliLogData} = post_ug_lib:check_log_progress_data_from_cli(["result=SUCCESS", 
								      "state=FINISHED"], 
								     ?LOG_LDN, 20),
   
    post_ug_lib:unpack_avli_log_data_from_cli(AvliLogData, AvliLogPath, AvliLogPath, 
					      ?SftpHost, ?SftpUser, ?SftpPassword),
    
    {ok, _} = post_ug_lib:check_avli_log_data_from_cli(AvliLogPath),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Export ESI after upgrade
    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    ct:pal("Export and Check ESI Log"),
    EsiLogPath=?config(priv_dir,Config),
    ct:pal("EsiLogPath: ~p~n", [EsiLogPath]),
    os:cmd("chmod 777 "++EsiLogPath), % else permission.
    
    EsiUri = sftp_uri(EsiLogPath),
    
    {ok, _} = post_ug_lib:fetch_esi("exportEsi", EsiUri, ?SftpPassword, ?LOG_LDN),
    {ok, EsiLogData} = post_ug_lib:check_log_progress_data_from_cli(["result=SUCCESS",
								     "state=FINISHED"], 
								    ?LOG_LDN, 80),

    post_ug_lib:unpack_esi_log_data_from_cli(EsiLogData, EsiLogPath, "resultInfo=", 
					     ?SftpHost, ?SftpUser, ?SftpPassword),
    
    
    case check_if_vc_board() of
	"yes"->
	    {ok, _} = post_ug_lib:check_esi_log_data_from_cli(EsiLogPath);
    	_-> 
	    continue
    end,	    
    
    ok = rct_cli:disconnect(?CLI_Session),

    NewConfig = 
	case RatType of
	    grat ->
		%%ct:pal("RatType = grat"),
		%% Add created up label to Config. Add upgrade times.
		%% Will be used in other upgrade actions.
		{_Saver, ConfigList} = ?config(saved_config, Config),
		%%ct:pal("Saver: ~p ConfigList: ~p ~n",[Saver, ConfigList]),
		%%NewConfig = ConfigList,
		{save_config, ConfigList};
	    wrat ->
		%%ct:pal("RatType = wrat"),
		{_Saver, ConfigList} = ?config(saved_config, Config),
		%%ct:pal("Saver: ~p ConfigList: ~p ~n",[Saver, ConfigList]),
		%%NewConfig = ConfigList,
		{save_config, ConfigList};
	    lrat ->
		%%ct:pal("RatType = lrat"),
		{_Saver, ConfigList} = ?config(saved_config, Config),
		%%ct:pal("Saver: ~p ConfigList: ~p ~n",[Saver, ConfigList]),
		%%NewConfig = ConfigList,
		{save_config, ConfigList};
	    tcu ->
		ct:pal("RatType = tcu"),
		{_Saver, ConfigList} = ?config(saved_config, Config),
		%%ct:pal("Saver: ~p ConfigList: ~p ~n",[Saver, ConfigList]),
		%%NewConfig = ConfigList,
		{save_config, ConfigList};
	    _Else4 ->
		ok
	end,
    
    ct:pal("after_ug NewConfig: ~p",[NewConfig]),
    NewConfig.
  
%%%--------------------------------------------------------------------
%%% @doc
%%% Create. <br/>
%%% @spec mod_to_up(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
mod_to_up(Config) ->

    ct:pal("Config:~n~p~n",[Config]),
    
    ok = rct_cli:connect(?CLI_Session),
    %% %% Get rat type installed on node.
    RatType = get_rat_type(),

    %% check if Sec Or Unsec board
    SecOrUnsec = check_if_vc_board(),
    ct:log("SecOrUnsec: ~p ~n", [SecOrUnsec]),

    UP = 
	case RatType of
	    RatType when RatType == wrat; 
			 RatType == lrat;
			 RatType == grat;
			 RatType == tcu ->  
		nodeUp;
	    _RatType  ->
		csUp
	end,

    ct:log("#Build valid to up. ~n"
	   "Create cxps that shall be used for UG."),
    
    NewConfig = 
	case {SecOrUnsec, UP} of
	    {"yes", nodeUp} -> 
		NodeUP = post_ug_lib:get_current_up_from_cli(),
		ct:pal("Current SwVersion on Node:~n~p~n",[NodeUP]),

		post_ug_lib:build_valid_full_ug_package(?UG_Session, 
							?DC_UP_NAME, NodeUP, 1),
		
		Config;
	    {"no", nodeUp} -> 
		NodeUP = swm_test_lib:get_sw_version(?NC_Session),
		ct:pal("Current SwVersion on node:~n~p~n",[NodeUP]),
		
		post_ug_lib:build_valid_full_ug_package(?UG_Session, 
							?DC_UP_NAME, NodeUP, 1),
		
		Config;
	    {_, csUp} -> 
		swm_test_lib:build_valid_ug_packakage(?NC_Session),
		swm_test_lib:set_housekeeping_delay_variable(rpc_1),
		Config
	end,
    
    ok = rct_cli:disconnect(?CLI_Session),

    ct:pal("Add NewConfig: ~p",[NewConfig]),
    NewConfig.
 
%%%--------------------------------------------------------------------
%%% @doc
%%% Create. <br/>
%%% @spec create(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
create(Config) ->

    ct:pal("Config:~n~p~n",[Config]),
    
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    From_Label = swm_test_lib:get_sw_version(?NC_Session),

    %%%%
    %% Create UG
    %%%%
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),

    
    %% Get rat type installed on node.
    RatType = get_rat_type(),

    %% Check if Sec Or Unsec board
    SecOrUnsec = check_if_vc_board(),
    ct:log("SecOrUnsec: ~p ~n", [SecOrUnsec]),
    

    UP = 
	case RatType of
	    RatType when RatType == wrat;
			 RatType == lrat;
			 RatType == grat;
			 RatType == tcu ->  
		nodeUp;
	    _Other ->
		csUp		     
	end,
    
    case {SecOrUnsec, UP} of
	{"yes", nodeUp} -> 
	    
	    ok = swm_test_lib:up_create_generic(?NC_Session,
						?SftpHost,
						?SftpUser,
						?SftpPassword,
						UGPath,
						MeId),

	    case swm_test_lib:wait_for_swm_progress_result(?NC_Session, MeId, no_check) of
		[{"SUCCESS" = Result,
		  "createUpgradePackage" = ActionName,
		  ProgressInfo,
		  ResultInfo,
		  _State,
		  _ProgReport}] ->
		    ct:log("result:~p~n"
			   "actionName:~p~n"
			   "progressInfo:~p~n"
			   "resultInfo:~p~n",[Result,
					      ActionName,
					      ProgressInfo,
					      ResultInfo]),
		    ok;
		Result ->
		    ResultInfo =dummy,
		    ct:pal("createUpgradePackage: ~p",[Result]),
		    ct:fail(Result)
	    end,

	    Label = get_up_package(ResultInfo),
	    ct:pal("Label: ~p", [Label]),

	    %% Add created up label to Config.
	    %% Will be used in other upgrade actions.
	    NewConfig = [{from_label, From_Label},
			 {uplabel, Label}],
	    {save_config, NewConfig};
	{"no", nodeUp} ->	    
	    ok = swm_test_lib:up_create_generic(?NC_Session,
						?SftpHost,
						?SftpUser,
						?SftpPassword,
						UGPath,
						MeId),

	    case swm_test_lib:wait_for_swm_progress_result(?NC_Session, MeId, no_check) of
		[{"SUCCESS" = Result,
		  "createUpgradePackage" = ActionName,
		  ProgressInfo,
		  ResultInfo,
		  _State,
		  _ProgReport}] ->
		    ct:log("result:~p~n"
			   "actionName:~p~n"
			   "progressInfo:~p~n"
			   "resultInfo:~p~n",[Result,
					      ActionName,
					      ProgressInfo,
					      ResultInfo]),
		    ok;
		Result ->
		    ResultInfo =dummy,
		    ct:pal("createUpgradePackage: ~p",[Result]),
		    ct:fail(Result)
	    end,

	    Label = get_up_package(ResultInfo),
	    ct:pal("Label: ~p", [Label]),
	    %% Add created up label to Config.
	    %% Will be used in other upgrade actions.
	    NewConfig = [{from_label, From_Label},
			 {uplabel, Label}],
	    {save_config, NewConfig};	   
	_  ->
	    swm_test_lib:ug_create_match_result(?NC_Session,
						"SUCCESS",
						?SftpHost,
						?SftpUser,
						?SftpPassword,
						UGPath,
						MeId)
    end.

%%%--------------------------------------------------------------------
%%% @doc
%%% Prepare. <br/>
%%% @spec prepare(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
prepare(Config) ->
    ct:pal("Config:~n~p~n",[Config]),
    %%%%
    %% Prepares an upgrade package,
    %% which means downloading a complete UP
    %%%%
    perform_ug_action(Config, prepare),

    %% Check if Sec Or Unsec board
    SecOrUnsec = check_if_vc_board(),
    ct:log("SecOrUnsec: ~p ~n", [SecOrUnsec]),
   
    %% Get rat type installed on node.
    RatType = get_rat_type(),
  
    UP = 
	case RatType of
	    RatType when RatType == wrat;
			 RatType == lrat;
			 RatType == grat;
			 RatType == tcu ->  
		nodeUp;
	    _Other ->
		csUp
	end,
   
    case {SecOrUnsec, UP}  of
	{"yes", nodeUp}  -> 
	    %% Add created up label to Config. 
	    %% Will be used in other upgrade actions.
	    {_Saver, ConfigList} = ?config(saved_config, Config),
	    NewConfig = ConfigList,
	    ct:pal("Add NewConfig: ~p",[NewConfig]),
	    {save_config, NewConfig};
	{"no", nodeUp}  -> 
	    %% Add created up label to Config. 
	    %% Will be used in other upgrade actions.
	    {_Saver, ConfigList} = ?config(saved_config, Config),
	    NewConfig = ConfigList,
	    ct:pal("Add NewConfig: ~p",[NewConfig]),
	    {save_config, NewConfig};
	_  ->
	    ok 
    end.

%%%--------------------------------------------------------------------
%%% @doc
%%% verify. <br/>
%%% @spec verify(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
verify(Config) ->
    ct:pal("Config:~n~p~n",[Config]),
    %%%%
    %% Verifies an upgrade package.
    %%%%
    perform_ug_action(Config, verify),

    %% Check if Sec Or Unsec board
    SecOrUnsec = check_if_vc_board(),
    ct:log("SecOrUnsec: ~p ~n", [SecOrUnsec]),

    %% Get rat type installed on node.
    RatType = get_rat_type(),
    
    UP = 
	case RatType of
	    RatType when RatType == wrat;
			 RatType == lrat;
			 RatType == grat;
			 RatType == tcu ->  
		nodeUp;
	    _Other ->
		csUp		     
	end,
    
    case {SecOrUnsec, UP}  of
	{"yes",nodeUp}  -> 
	    %% Add created up label to Config. A
	    %% Will be used in other upgrade actions.
	    {_Saver, ConfigList} = ?config(saved_config, Config),
	    NewConfig = ConfigList,
	    ct:pal("Add NewConfig: ~p",[NewConfig]),
	    {save_config, NewConfig};
	{"no",nodeUp}  -> 
	   %% Add created up label to Config. A
	    %% Will be used in other upgrade actions.
	    {_Saver, ConfigList} = ?config(saved_config, Config),
	    NewConfig = ConfigList,
	    ct:pal("Add NewConfig: ~p",[NewConfig]),
	    {save_config, NewConfig};
	_  ->
	    ok 
    end.

%%%--------------------------------------------------------------------
%%% @doc
%%% activate. <br/>
%%% @spec activate(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
activate(Config) ->
    ct:pal("Config:~n~p~n",[Config]),
    %%%%
    %% Activates an upgrade package.
    %% During activate the node will reboot.
    %%%%
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    net_kernel:disconnect(ErlNode),

    perform_ug_action(Config, activate),

    %% Check if Sec Or Unsec board
    SecOrUnsec = check_if_vc_board(),
    ct:log("SecOrUnsec: ~p ~n", [SecOrUnsec]),

    %% Get rat type installed on node.
    RatType = get_rat_type(),
    
    UP = 
	case RatType of
	    RatType when RatType == wrat;
			 RatType == lrat;
			 RatType == grat;
			 RatType == tcu ->  
		nodeUp;
	    _Other ->
		csUp		    
	end,

    case {SecOrUnsec, UP}  of
	{"yes",nodeUp}  -> 
	    %% Add created up label to Config. A
	    %% Will be used in other upgrade actions.
	    {_Saver, ConfigList} = ?config(saved_config, Config),
	    NewConfig = ConfigList,
	    ct:pal("Add NewConfig: ~p",[NewConfig]),
	    {save_config, NewConfig};
	{"no",nodeUp}  -> 
	       %% Add created up label to Config. A
	    %% Will be used in other upgrade actions.
	    {_Saver, ConfigList} = ?config(saved_config, Config),
	    NewConfig = ConfigList,
	    ct:pal("Add NewConfig: ~p",[NewConfig]),
	    {save_config, NewConfig};
	_  ->
	    ok 
    end.

%%%--------------------------------------------------------------------
%%% @doc
%%% confirm. <br/>
%%% @spec confirm(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
confirm(Config) ->
    ct:pal("Config:~n~p~n",[Config]),
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),


    %%%%
    %% Get Latest UP
    %%%%
    Label = get_latest_up(Config),

    ct:pal("Perform ug confirm~n",[]),
    swm_test_lib:ug_confirm(?NC_Session,
			    Label,
			    MeId),

    %% Check if Sec Or Unsec board
    SecOrUnsec = check_if_vc_board(),
    ct:log("SecOrUnsec: ~p ~n", [SecOrUnsec]),

    %% Get rat type installed on node.
    RatType = get_rat_type(),

    UP = 
	case RatType of
	    RatType when RatType == wrat;
			 RatType == lrat;
			 RatType == grat;
			 RatType == tcu ->  
		nodeUp;
	    _Other ->
		csUp
	end,
    
    case {SecOrUnsec, UP}  of
	{"yes",nodeUp}  -> 
	    %% Add created up label to Config. A
	    %% Will be used in other upgrade actions.
	    {_Saver, ConfigList} = ?config(saved_config, Config),
	    NewConfig = ConfigList,
	    ct:pal("Add NewConfig: ~p",[NewConfig]),
	    {save_config, NewConfig};
	{"no",nodeUp}  -> 
	    %% Add created up label to Config. A
	    %% Will be used in other upgrade actions.
	    {_Saver, ConfigList} = ?config(saved_config, Config),
	    NewConfig = ConfigList,
	    ct:pal("Add NewConfig: ~p",[NewConfig]),
	    {save_config, NewConfig};
	_  ->
	    ok 
    end.

%%%--------------------------------------------------------------------
%%% @doc
%%% remove. <br/>
%%% @spec remove(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
remove(Config) ->
    ct:pal("Config:~n~p~n",[Config]),
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Get Latest UP
    %%%%
    Label = get_latest_up(Config),

    %%%%
    %% Remove the created up.
    %%%%
    ct:pal("Remove upgrade package: : ~s",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_Session,
					"SUCCESS",
					MeId,
					Label),

    ok.

%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------
get_latest_up(Config) ->
    %% [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    %% ct:pal("Label:~n~p~n",[Label]),


    %% Check if Sec Or Unsec board
    SecOrUnsec = check_if_vc_board(),
    ct:log("SecOrUnsec: ~p ~n", [SecOrUnsec]),


    %% Get rat type installed on node.
    RatType = get_rat_type(),

    UP = 
	case RatType of
	    RatType when RatType == wrat;
			 RatType == lrat;
			 RatType == grat;
			 RatType == tcu ->  
		nodeUp;
	    _Other ->
		csUp		    
	end,

    NewLabel =
	case {SecOrUnsec, UP}  of
	    {"yes",nodeUp}  -> 
		{_Saver, ConfigList} = ?config(saved_config, Config),
		{uplabel, Label} = lists:keyfind(uplabel, 1, ConfigList),
		Label;
	    {"no",nodeUp}  -> 
	    	{_Saver, ConfigList} = ?config(saved_config, Config),
		{uplabel, Label} = lists:keyfind(uplabel, 1, ConfigList),
		Label;
	    _ ->
		UPs = swm_test_lib:get_ups(?NC_Session),
		ct:log("UPs:~n~p~n",[UPs]),
		Label = swm_test_lib:get_highest_label(UPs),
		Label
	end,
    ct:log("Label:~n~p~n",[NewLabel]),
    NewLabel.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
perform_ug_action(Config, Action) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Get Latest UP
    %%%%
    Label = get_latest_up(Config),

    %%%%
    %% Perform ug action
    %%%%
    ct:pal("Perform ug action:~n~p~n",[Action]),
    swm_test_lib:ug_action_match_result(?NC_Session,
    					"SUCCESS",
    					Label,
    					MeId,
    					Action),
    Label.


%%%--------------------------------------------------------------------
%%% Description: 
%%%--------------------------------------------------------------------
wait_for_exp_alarm_to_exist(NC_Session, MeId, ExpAlarm) ->
    wait_for_exp_alarm_to_exist(NC_Session, MeId, ExpAlarm, 120000).

wait_for_exp_alarm_to_exist(_NC_Session, _MeId, _ExpAlarm, Timeout) when 
      Timeout < 0 ->
    ct:fail(" Expected alarm not exist within expected time.");

wait_for_exp_alarm_to_exist(NC_Session, MeId, ExpAlarm, Timeout) ->
    case swm_test_lib:is_alarm(NC_Session, MeId, ExpAlarm) of
	true ->
	    ok;
	false ->
	    ct:pal("Exp alarm not exist, wait and check again"),
	    timer:sleep(5000),
	    wait_for_exp_alarm_to_exist(NC_Session, MeId, ExpAlarm, 
					Timeout-5000)    
    end.


wait_for_exp_alarm_to_cease(NC_Session, MeId, ExpAlarm) ->
    wait_for_exp_alarm_to_cease(NC_Session, MeId, ExpAlarm, 300000).

wait_for_exp_alarm_to_cease(_NC_Session, _MeId, _ExpAlarm, Timeout) when 
      Timeout < 0 ->
    ct:fail(" Expected alarm not ceased within expected time.");

wait_for_exp_alarm_to_cease(NC_Session, MeId, ExpAlarm, Timeout) ->
    case swm_test_lib:is_alarm(NC_Session, MeId, ExpAlarm) of
	false ->
	    ok;
	true ->
	    ct:pal("Exp alarm not ceased, wait and check again"),
	    timer:sleep(5000),
	    wait_for_exp_alarm_to_cease(NC_Session, MeId, ExpAlarm, 
					 Timeout-5000)    
    end.


%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
sftp_uri(Path) ->
    Uri = "sftp://"++?SftpUser++"@"++?SftpHost++Path,
    ct:pal("SFTP URI: ~p~n", [Uri]),
    Uri.

%%%--------------------------------------------------------------------
%%% Description: Evaluate an os cmd and print both the command and result
%%%--------------------------------------------------------------------
cmd(Cmd) ->
    ct:pal("~s~n~s~n",[lists:flatten(Cmd), R = os:cmd(Cmd)]),
    R.
%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------	
check_if_vc_board()->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
    Secure_board.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------	
%% Get rat type installed on node. Only works when testsuite is part of jenkins
get_rat_type()-> 
    RatType = ct:get_config({jenkins_config, installed_type}),
    ct:log("RatType: ~p", [RatType]),
    RatType.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
%% get_nr_from_saved_config(Config) ->

%%     {_Saver, ConfigList} = ?config(saved_config, Config),
%%     {nr, Nr} = lists:keyfind(nr, 1, ConfigList),
%%     Nr.

%%%--------------------------------------------------------------------
%%% Description: Get latest UP
%%%--------------------------------------------------------------------
get_up_package(ResultInfo) ->
    UPLabel = lists:last(string:tokens(ResultInfo,"=")),
    ct:pal("UPLabel: ~p",[UPLabel]),
    UPLabel.

%%%--------------------------------------------------------------------
%%% Description: Get ldap server ipadress
%%%--------------------------------------------------------------------
get_ldap_server_ipaddress()->
    [{host,LdapIpAddress}]=ct:get_config(ldap_server),
    LdapIpAddress.

%%%--------------------------------------------------------------------
%%% Description:
%%%--------------------------------------------------------------------
%% configure_node_ipv4address_suppot_via_cli() ->

%%     {ok, _} = post_ug_lib:configure_tnport(
%% 		"ManagedElement=1,Equipment=1,FieldReplaceableUnit=1",
%% 		"TnPort=TN_A"),	

%%     {ok, _} = post_ug_lib:configure_ethernetport(
%% 		"ManagedElement=1,Transport=1,EthernetPort=1",
%% 		["administrativeState=UNLOCKED",
%% 		 "encapsulation=ManagedElement=1,Equipment=1,FieldReplaceableUnit=1,TnPort=TN_A"]),
    
%%     {ok, _} = post_ug_lib:configure_vlanport(
%% 		"ManagedElement=1,Transport=1,VlanPort=1",
%% 		["encapsulation=ManagedElement=1,Transport=1,EthernetPort=1",
%% 		 "vlanId=1000"]),
    
%%     {ok, _} = post_ug_lib:configure_ipv4address(
%% 		"ManagedElement=1,Transport=1,Router=1",
%% 		"InterfaceIPv4=1",
%% 		["encapsulation=ManagedElement=1,Transport=1,VlanPort=1"],
%% 		"AddressIPv4=1",
%% 		["address=10.86.144.91/24"]),
    
%%     {ok, _} = post_ug_lib:configure_routetableIPv4static(
%% 		"ManagedElement=1,Transport=1,Router=1",
%% 		"RouteTableIPv4Static=1",
%% 		"Dst=1",
%% 		["dst=128.0.0.0/2"],
%% 		"NextHop=1",
%% 		["address=10.86.144.1","adminDistance=1"],
%% 		"Dst=2",
%% 		["dst=147.214.0.0/16"],
%% 		"NextHop=1",
%% 		["address=10.86.144.1","adminDistance=1"]).

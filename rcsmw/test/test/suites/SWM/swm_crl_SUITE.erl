%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_crl_SUITE.erl %
%%% @author eransbn
%%% @copyright Ericsson AB 2017
%%% @version /main/R8A/R10A/R12A/1
%%%
%%% @doc == Test Suite for testing various software management related stuff ==
%%% This Test Suite can be used on target unsecure unlocked board.
%%% <br/><br/>
%%% @end

-module(swm_crl_SUITE).

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%%
%% @doc
%% Initialization before the suite.
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @end
%%% ----------------------------------------------------------

-include_lib("common_test/include/ct.hrl").
-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).
-export([integration_uc1/1,
	 integration_uc2/1,
	 migration_uc1/1,
	 migration_uc2/1,
	 revocation_uc1/1,
	 revocation_uc2_power/1,
	 revocation_uc3_revoctimer/1,
	 revocation_uc4_installReject/1,
	 revocation_uc5_upgradeReject/1,
	 revocation_uc6_timeoutpower/1,
	 backup_revocation_uc1/1,
	 hardfactory_revocation_uc1/1,
	 tryy/1
	]).

-define(TAFILE, "ta_test_file"). 
-define(TADIRTARGET, "/rcs/ta_test").
-define(TADIRNOTTARGET,"/vobs/rcs/test/RCT_CRX901275/test/suites/SWM/bin").
-define(BUILDUPSCRIPT, "/vobs/rcs/test/RCT_CRX901275/test/suites/SWM/bin/swm_buildup_revoc.sh").
-define(NC_Session, nc1).
-define(BU_Name_1, "bu_test_4").


suite()->

    [{timetrap, {minutes, 600}}, % 10 hours
     {ct_hooks, [{rct_scp, [{1, scp}]},
		 {rct_power,power},
		 {cth_conn_log,[]},
		 {rct_consserv,cs1},
		 {rct_rs232, console1},
		 {rct_logging, {all,[{erlang,{["ERROR REPORT","CRASH REPORT"],["The system is in the software signing certificate revocation grace period, which expires"]}}]}},
		 {rct_netconf, nc1},
		 {rct_rpc, rpc_1},		 
		 {rct_upgrade,ug1},
		 {rct_coli, {coli, [manual_connect]}},
		 {rct_ssh,{ssh,[manual_connect]}}			  
		]}]. 
%% @hidden
init_per_suite(Config) ->
    Hw = ct:get_config({test_nodes,1}),
    TftpDir =  ct:get_config({Hw,tftpboot}),

    OriginalUpPath = save_original_up(TftpDir,"OriginalUp", Config),
    UpNewCrl = build_up(nc1, ug1, "NewCrlUp","1", "noRevoc", TftpDir), 
    UpRevCrl =  build_up(nc1, ug1, "RevCrlUp","2", "revoc", TftpDir), 
    UpRevCrl2 =  build_up(nc1, ug1, "RevCrlUp2","3", "revoc", TftpDir), 
    UpNewCrl2 = build_up(nc1, ug1, "NewCrlUp2","4", "noRevoc", TftpDir), 

    ct:pal("OriginalUpPath ~p",[OriginalUpPath]),
    ct:pal("UpNewCrl ~p",[UpNewCrl]),
    ct:pal("UpRevCrl ~p",[UpRevCrl]),
    ct:pal("UpRevCrl2 ~p",[UpRevCrl2]),
    ct:pal("UpNewCrl2 ~p",[UpNewCrl2]),
  
    [{upnewcrl, UpNewCrl},{upnewcrl2, UpNewCrl2}, {uprevcrl, UpRevCrl}, 
     {uprevcrl2, UpRevCrl2},{orginluppath, OriginalUpPath}|
     Config].  


%%--------------------------------------------------------------------
%% @doc
%% Cleanup after the suite.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case
%%
%% TestCase - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_testcase(TestCase, Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_testcase(TestCase, Config) ->
    ct:print("Now running ~w~n",[TestCase]),

    Up = rct_rpc:call(rpc_1, swmI, get_current_up_metadata, [], 10000,
		      noprint),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    net_kernel:disconnect(ErlNode),
    ct:pal("Current UP:~n~p~n",[Up]),

    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @end
%%--------------------------------------------------------------------
end_per_testcase(TestCase, Config) ->
    ct:pal("Testcase ~p",[TestCase]),

    case proplists:get_value(tc_status, Config) of
    	ok -> 
    	    ok;
    	R -> ct:log("test case ~p",[R]),
    	     take_out_node_from_pool(Config)
    end,
    ok.
%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() ->
    [integration_uc1, 
     integration_uc2,
     migration_uc1, 
     migration_uc2,
     revocation_uc1,
     revocation_uc2_power,
     revocation_uc3_revoctimer,
     revocation_uc4_installReject,
     revocation_uc5_upgradeReject,
     revocation_uc6_timeoutpower,
     backup_revocation_uc1,
     hardfactory_revocation_uc1
    ].

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
%%% R1A/1      2016-10-12 eransbn     Created


%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------
 tryy(Config)->

 %% InstallUp = proplists:get_value(upnewcrl, Config),
 %%    UgPathUp = proplists:get_value(uprevcrl, Config),
    MeId = swm_test_lib:get_me_id(?NC_Session),
% BackupInstall = swm_br_lib:get_all_backups(?NC_Session, MeId),
%ct:pal("adf ~p",[BackupInstall]),
create_backup(Config, ?BU_Name_1),

 Backups = swm_br_lib:get_all_backups(?NC_Session, MeId),
ct:pal("Backups ~p length ~p",[Backups, length(Backups)]),

    BuId = swm_br_lib:get_correct_buid(?BU_Name_1, Backups), %% in a list.
    ct:pal("Selected buId: ~p, to restore.~n",[BuId]),



 %%    erase_flash(Config),
 %%    put_ta_on_node(),
 %%    board_restore(),
 %%    rcstprep(InstallUp),
 %%    install_sw(),
 %%InstallUp = proplists:get_value(upnewcrl, Config),
  %%  UgPathUp = proplists:get_value(uprevcrl, Config),
    %% UgPathUp2 = proplists:get_value(uprevcrl2, Config),

    %% ct:log("upgrade while revocation timer still counting"),
    %% upgradeprep(UgPathUp2),
    %% create_fail(Config),

 %% Revoctimer = rct_rpc:call(rpc_1, swmServer, get_sign_cert_timer, [], 1000,
 %% 		      print),
 %%    ct:pal("Revoctimer ~p",[Revoctimer]),
    %% Reply = rct_rpc:call(rpc_1, swmServer, signing_cert_timer, [120], 1000,
    %% 		      print),
    %% ct:pal("Revocation timer ~p",[Reply]),
    



    ok.



%%%--------------------------------------------------------------------
%%% @doc Install to AR capaple up and no soft fuse in hw
%%% @end
%%%--------------------------------------------------------------------
integration_uc1(Config)->
    ct:log("Install to AR capaple up and no soft fuse in hw"),
    erase_flash(Config),
    board_restore(),
    ct:pal("load up ~p",[proplists:get_value(orginluppath, Config)]),
    rcstprep(proplists:get_value(orginluppath, Config)),
    install_sw(),
    ok.

%%%--------------------------------------------------------------------
%%% @doc Install the node with new CRL and has been loaded with AR capable UP
%%% @end
%%%--------------------------------------------------------------------
integration_uc2(Config) ->
    ct:log("Install the node with new CRL and has been loaded with AR capable UP"),

    %%check that up are installed
    site_config_complete([]),
    put_ta_on_node(),
    board_restore(),
    ct:pal("load up ~p",[proplists:get_value(upnewcrl, Config)]),
    rcstprep(proplists:get_value(upnewcrl, Config)),
    install_sw(),
    ok.
%%%--------------------------------------------------------------------
%%% @doc Upgrade from legacy up to ar capable up, no soft fuse in 'HW'
%%%      and same CRL
%%% @end
%%%--------------------------------------------------------------------
migration_uc1(Config) ->
    ct:log("Upgrade from legacy up to ar capable up, no soft fuse in 'HW'~n"
	   "and same CRL"),
   %% LegacyUp = ct:get_config({jenkins_config, upgrade_from_cxp}),
    LegacyUp = "CXS101549_8-R12A17",

    ct:log("LegacyUp ~p",[LegacyUp]),
    UgPath = proplists:get_value(orginluppath, Config),

    site_config_complete([]),
    erase_flash(Config),
    remove_ta_from_node(),
    board_restore(),
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Prep_Cmd = "rcstprep.sh "++Hw++" "++LegacyUp,
    ct:pal("Prep_Cmd: ~p", [Prep_Cmd]),
    os:cmd(Prep_Cmd),

    install_sw(),%% legacy up
    timer:sleep(10000),

    %%upgrade to AR UP and same crl
    swm_upg_crl_lib:run_upgrade(Config, ?NC_Session, UgPath),
    case site_config_complete([]) of
    	ok -> ok;
    	{error,String}  -> 
    	    ct:fail(String)
    end,
    ok.
%%%--------------------------------------------------------------------
%%% @doc Upgrade from legacy up to ar capable up, no soft fuse in 'HW'
%%%      and new CRL
%%% @end
%%%--------------------------------------------------------------------
migration_uc2(Config) ->
    ct:log("Upgrade from legacy up to ar capable up, no soft fuse in 'HW'~n"
	   "and new CR"),
   %% LegacyUp = ct:get_config({jenkins_config, upgrade_from_cxp}),
    LegacyUp = "CXS101549_8-R12A17",

    ct:log("LegacyUp ~p",[LegacyUp]),
    UgPath = proplists:get_value(upnewcrl, Config),

    site_config_complete([]),
    erase_flash(Config),
   %%remove_ta_from_node(),
    board_restore(),
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Prep_Cmd = "rcstprep.sh "++Hw++" "++LegacyUp,
    ct:pal("Prep_Cmd: ~p", [Prep_Cmd]),
    os:cmd(Prep_Cmd),

    install_sw(),%% legacy up
send_coli_2_command("/misc/authlevel disabled", "/labonly/swm/enable-anti-rollback on"),
    put_ta_on_node(),

    %%upgrade to AR UP and new crl
   swm_upg_crl_lib:run_upgrade(Config, ?NC_Session, UgPath),
    case site_config_complete([]) of
    	ok -> ok;
    	{error,String}  -> 
    	    ct:fail(String)
    end,

    ok.
%%%--------------------------------------------------------------------
%%% @doc Upgrade from AR capable up to ar capable up, soft fuse in 'HW'
%%%      and new CRL
%%% @end
%%%--------------------------------------------------------------------
revocation_uc1(Config)->  
    ct:log("Upgrade from AR capable up to ar capable up, soft fuse in 'HW'~n"
	   "and new CRL"),
    MeId = swm_test_lib:get_me_id(?NC_Session),
    InstallUp = proplists:get_value(upnewcrl, Config),
    UgPathUp = proplists:get_value(uprevcrl, Config),

    put_ta_on_node(),
    rcstprep(InstallUp),
    board_restore(),
    install_sw(),
    send_coli_2_command("/misc/authlevel disabled", "/labonly/swm/enable-anti-rollback on"),

    %%upgrade to AR UP and revoc UP
    swm_upg_crl_lib:run_upgrade(Config, ?NC_Session, UgPathUp),
    case site_config_complete([]) of
    	ok -> ok;
    	{error,String}  -> 
    	    ct:fail(String)
    end,


    %%check swm
    UPs = swm_test_lib:get_ups(?NC_Session),
    ct:pal("Ups ~p",[UPs]),
    %% Highest label after Upgrade.
    LatestLabel = swm_test_lib:get_highest_label(UPs),
    ct:pal("Revoc up ~p",[LatestLabel]),
    PrevLabel = lists:delete(LatestLabel, UPs),
    "COMMIT_COMPLETED" =  
	swm_test_lib:get_up_state(?NC_Session, MeId, LatestLabel),
    "PREPARE_COMPLETED"  =  swm_test_lib:get_up_state(?NC_Session, MeId, PrevLabel),
  
    %% workaround
    ct:pal("timer sleep 2 min workaround"),
    timer:sleep(120000),
    
    %%Confirme revocation up
    ExpectString = "New software signing certificates are included in the "
	"current UP and will be auto confirmed at",
    send_coli("/swm/signingcertupdate",ExpectString, 30 ),


    send_coli("/swm/signingcertupdate confirm"),
    %% timer:sleep(20000),
    ok = loop_nr_up(1, 100),
    wait_for_nr_of_exp_bu(1, MeId),

    ok.


%%%--------------------------------------------------------------------
%%% @doc Upgrade from AR capable up to ar capable up, soft fuse in 'HW'
%%%      and new CRL. Do power off/on while still counting
%%% @end
%%%--------------------------------------------------------------------
revocation_uc2_power(Config)->  

    InstallUp = proplists:get_value(upnewcrl, Config),
    UgPathUp = proplists:get_value(uprevcrl, Config),
    MeId = swm_test_lib:get_me_id(?NC_Session),

    erase_flash(Config),
    board_restore(),
    rcstprep(InstallUp),
    install_sw(),
    send_coli_2_command("/misc/authlevel disabled", "/labonly/swm/enable-anti-rollback on"),

    %%upgrade to AR UP and revoc up
    swm_upg_crl_lib:run_upgrade(Config, ?NC_Session, UgPathUp),
    case site_config_complete([]) of
    	ok -> ok;
    	{error,String}  -> 
    	    ct:fail(String)
    end,
 
    %% Highest label after Upgrade.
    UPs = swm_test_lib:get_ups(?NC_Session),
    ct:pal("Ups ~p",[UPs]),
    LatestLabel = swm_test_lib:get_highest_label(UPs),
    ct:pal("Revoc up ~p",[LatestLabel]),
    PrevLabel = lists:delete(LatestLabel, UPs),
    "COMMIT_COMPLETED" =  
	swm_test_lib:get_up_state(?NC_Session, MeId, LatestLabel),
    "PREPARE_COMPLETED"  =  swm_test_lib:get_up_state(?NC_Session, MeId, PrevLabel),
    %% workaround
    ct:pal("timer sleep 2 min workaround"),
    timer:sleep(120000),
    
    %%Confirm revocation up
    ExpectString = "New software signing certificates are included in the "
	"current UP and will be auto confirmed at",
    ColiReply = send_coli("/swm/signingcertupdate", ExpectString, 30),

    %%power off/on
      case rct_power:cycle(power) of
	ok -> ok;
	Reply -> ct:fail("Power fail reason: ~p",[Reply])
    end,
    case site_config_complete([]) of
    	ok -> ok;
    	{error,String2}  -> 
    	    ct:fail(String2)
    end,
    ColiReply2 = send_coli("/swm/signingcertupdate", ExpectString, 30),
    case ColiReply2 =:= ColiReply of
	true -> ok;
	false -> ct:fail("/swm/signingcertupdate not same before and after power cycle")
			 
    end,

   %% ok = loop_nr_up(1, 100),

    ok. 

 
%%%--------------------------------------------------------------------
%%% @doc Upgrade from AR capable up to ar capable up, soft fuse in 'HW'
%%%      and new CRL and revacation UP. The revocation timer timeout
%%% @end
%%%--------------------------------------------------------------------
revocation_uc3_revoctimer(Config)->  
    ct:log("Upgrade from AR capable up to ar capable up, soft fuse in 'HW'~n"
	   "and new CRL and revacation UP. The revocation timer timeout"),

    InstallUp = proplists:get_value(upnewcrl, Config),
    UgPathUp = proplists:get_value(uprevcrl, Config),
    MeId = swm_test_lib:get_me_id(?NC_Session),

    erase_flash(Config),
    put_ta_on_node(),
    board_restore(),
    rcstprep(InstallUp),
    install_sw(),
send_coli_2_command("/misc/authlevel disabled", "/labonly/swm/enable-anti-rollback on"),

    %%Set revocation timer
    ok = rct_rpc:call(rpc_1, swmServer, signing_cert_timer, [120], 1000,
		      print),

    %%upgrade to AR UP and revoc up
    swm_upg_crl_lib:run_upgrade(Config, ?NC_Session, UgPathUp),
    case site_config_complete([]) of
    	ok -> ok;
    	{error,String}  -> 
    	    ct:fail(String)
    end,

    %% Highest label after Upgrade.
    UPs = swm_test_lib:get_ups(?NC_Session),
    ct:pal("Ups ~p",[UPs]),
    LatestLabel = swm_test_lib:get_highest_label(UPs),
    ct:pal("Revoc up ~p",[LatestLabel]),
    PrevLabel = lists:delete(LatestLabel, UPs),
    "COMMIT_COMPLETED" =  
	swm_test_lib:get_up_state(?NC_Session, MeId, LatestLabel),
    "PREPARE_COMPLETED"  =  swm_test_lib:get_up_state(?NC_Session, MeId, PrevLabel),


    RevocTimer = 1000 * 60 * 3,
    ct:log("Sleep ~p s",[RevocTimer/60]),
    timer:sleep(RevocTimer),
    ok = loop_nr_up(1, 100), 


    %%temp eransbn
    erase_flash(Config),
    remove_ta_from_node(),
    ok.
%% %%%--------------------------------------------------------------------
%% %%% @doc Upgrade from AR capable up to ar capable up, soft fuse in 'HW'
%% %%%      and new CRL and revacation UP. Do board restore and install initial 
%% %%%      up, that will fail.
%% %%% @end
%% %%%--------------------------------------------------------------------
revocation_uc4_installReject(Config) ->

    InstallUp = proplists:get_value(upnewcrl, Config),
    UgPathUp = proplists:get_value(uprevcrl, Config),
    MeId = swm_test_lib:get_me_id(?NC_Session),

   %% erase_flash(Config),
    put_ta_on_node(),
    board_restore(),
    rcstprep(InstallUp),
    install_sw(),
 send_coli_2_command("/misc/authlevel disabled", "/labonly/swm/enable-anti-rollback on"),

    swm_upg_crl_lib:run_upgrade(Config, ?NC_Session, UgPathUp),
    case site_config_complete([]) of
    	ok -> ok;
    	{error,String}  -> 
    	    ct:fail(String)
    end,
    %%check swm
    UPs = swm_test_lib:get_ups(?NC_Session),
    ct:pal("Ups ~p",[UPs]),
    %% Highest label after Upgrade.
    LatestLabel = swm_test_lib:get_highest_label(UPs),
    ct:pal("Revoc up ~p",[LatestLabel]),
    PrevLabel = lists:delete(LatestLabel, UPs),
    "COMMIT_COMPLETED" =  
	swm_test_lib:get_up_state(?NC_Session, MeId, LatestLabel),
    "PREPARE_COMPLETED"  =  swm_test_lib:get_up_state(?NC_Session, MeId, PrevLabel),
  
    %% workaround
    ct:pal("timer sleep 2 min workaround"),
    timer:sleep(120000),
    
    %%Confirm revocation up
    ExpectString = "New software signing certificates are included in the "
	"current UP and will be auto confirmed at",
    send_coli("/swm/signingcertupdate",ExpectString, 30 ),


    send_coli("/swm/signingcertupdate confirm"),
    %% timer:sleep(20000),
    ok = loop_nr_up(1, 100),

    %%Installation initial up and it will fail, 
    board_restore(),
    rcstprep(InstallUp),
    install_fail_sw("Signature check failed for software package"),
  
    %%install revocation up
    rcstprep(UgPathUp),
    install_sw(),
    erase_flash(Config),
    remove_ta_from_node(),

    ok.
%% %%%--------------------------------------------------------------------
%% %%% @doc Upgrade from AR capable up to ar capable up, soft fuse in 'HW'
%% %%%      and new CRL and revacation UP. Do upgrade while revocation timer 
%% %%%      still counting.
%% %%% @end
%% %%%--------------------------------------------------------------------
revocation_uc5_upgradeReject(Config) ->

    InstallUp = proplists:get_value(upnewcrl, Config),
    UgPathUp = proplists:get_value(uprevcrl, Config),
    UgPathUp2 = proplists:get_value(uprevcrl2, Config),

    MeId = swm_test_lib:get_me_id(?NC_Session),

   %% erase_flash(Config),
    put_ta_on_node(),
    board_restore(),
    rcstprep(InstallUp),
    install_sw(),
 send_coli_2_command("/misc/authlevel disabled", "/labonly/swm/enable-anti-rollback on"),

    swm_upg_crl_lib:run_upgrade(Config, ?NC_Session, UgPathUp),
    case site_config_complete([]) of
    	ok -> ok;
    	{error,String}  -> 
    	    ct:fail(String)
    end,

    %%check swm
    UPs = swm_test_lib:get_ups(?NC_Session),
    ct:pal("Ups ~p",[UPs]),
    %% Highest label after Upgrade.
    LatestLabel = swm_test_lib:get_highest_label(UPs),
    ct:pal("Revoc up ~p",[LatestLabel]),
    PrevLabel = lists:delete(LatestLabel, UPs),
    "COMMIT_COMPLETED" =  
	swm_test_lib:get_up_state(?NC_Session, MeId, LatestLabel),
    "PREPARE_COMPLETED"  =  swm_test_lib:get_up_state(?NC_Session, MeId, PrevLabel),
  
    %% workaround
    ct:pal("timer sleep 2 min workaround"),
    timer:sleep(120000),

    %%Upgrad fail
    ct:log("upgrade while revocation timer still counting"),
    upgradeprep(UgPathUp2),
    create_fail(Config),

    %%Confirme revocation up
    ExpectString = "New software signing certificates are included in the "
	"current UP and will be auto confirmed at",
    send_coli("/swm/signingcertupdate",ExpectString, 30 ),


    send_coli("/swm/signingcertupdate confirm"),
    %% timer:sleep(20000),
    ok = loop_nr_up(1, 100),
    upgradeprep(UgPathUp2),
    swm_upg_crl_lib:run_upgrade(Config, ?NC_Session, UgPathUp2),

    erase_flash(Config),
    remove_ta_from_node(),
    ok.
%% %%%--------------------------------------------------------------------
%% %%% @doc Upgrade from AR capable up to ar capable up, soft fuse in 'HW'
%% %%%      and new CRL and revacation UP. Power off on  while revocation timer 
%% %%%      still counting and it timeout will power off.
%% %%% @end
%% %%%--------------------------------------------------------------------
revocation_uc6_timeoutpower(Config) ->
    InstallUp = proplists:get_value(upnewcrl, Config),
    UgPathUp = proplists:get_value(uprevcrl, Config),  
    MeId = swm_test_lib:get_me_id(?NC_Session),

    put_ta_on_node(),
    board_restore(),
    rcstprep(InstallUp),
    install_sw(),
send_coli_2_command("/misc/authlevel disabled", "/labonly/swm/enable-anti-rollback on"),

    %%Set revocation timer
    RevocationTimer = 120,
    ok = rct_rpc:call(rpc_1, swmServer, signing_cert_timer, [RevocationTimer], 1000,
		      print),

    swm_upg_crl_lib:run_upgrade(Config, ?NC_Session, UgPathUp),
    case site_config_complete([]) of
    	ok -> ok;
    	{error,String}  -> 
    	    ct:fail(String)
    end,

    %%check swm
    UPs = swm_test_lib:get_ups(?NC_Session),
    ct:pal("Ups ~p",[UPs]),
    %% Highest label after Upgrade.
    LatestLabel = swm_test_lib:get_highest_label(UPs),
    ct:pal("Revoc up ~p",[LatestLabel]),
    PrevLabel = lists:delete(LatestLabel, UPs),
    "COMMIT_COMPLETED" =  
	swm_test_lib:get_up_state(?NC_Session, MeId, LatestLabel),
    "PREPARE_COMPLETED"  =  swm_test_lib:get_up_state(?NC_Session, MeId, PrevLabel),

    ct:log("Power off"),
    case rct_power:off(power) of
	ok -> ok;
	Reply -> ct:fail("Power fail reason: ~p",[Reply])
    end,
    SleepPower = RevocationTimer + 60,
    ct:log("Sleep ~p min before power on ", [SleepPower]),
    timer:sleep(SleepPower),

    case rct_power:on(power) of
	ok -> ok;
	Reply2 -> ct:fail("Power fail reason: ~p",[Reply2])
    end,
    case site_config_complete([]) of
	ok -> ok;
	{error,String2} -> 
	    ct:fail(String2)
    end,
    ok = loop_nr_up(1, 100),
    wait_for_nr_of_exp_bu(1, MeId),
    remove_ta_from_node(),

    ok.
backup_revocation_uc1(Config)->  
    InstallUp = proplists:get_value(upnewcrl, Config),
    UgPathUp = proplists:get_value(uprevcrl, Config),  
    MeId = swm_test_lib:get_me_id(?NC_Session),

    put_ta_on_node(),
    erase_flash(Config),

    board_restore(),
    rcstprep(InstallUp),
    install_sw(),
 send_coli_2_command("/misc/authlevel disabled", "/labonly/swm/enable-anti-rollback on"),

    create_backup(Config, ?BU_Name_1),

    swm_upg_crl_lib:run_upgrade(Config, ?NC_Session, UgPathUp),
    case site_config_complete([]) of
    	ok -> ok;
    	{error,String}  -> 
    	    ct:fail(String)
    end,

    %%check swm
    UPs = swm_test_lib:get_ups(?NC_Session),
    ct:pal("Ups ~p",[UPs]),
    %% Highest label after Upgrade.
    LatestLabel = swm_test_lib:get_highest_label(UPs),
    ct:pal("Revoc up ~p",[LatestLabel]),
    PrevLabel = lists:delete(LatestLabel, UPs),
    "COMMIT_COMPLETED" =  
	swm_test_lib:get_up_state(?NC_Session, MeId, LatestLabel),
    "PREPARE_COMPLETED" = swm_test_lib:get_up_state(?NC_Session, MeId, PrevLabel),

    %% workaround
    ct:pal("timer sleep 2 min workaround"),
    timer:sleep(120000),

    %%Confirm revocation up
    ExpectString = "New software signing certificates are included in the "
	"current UP and will be auto confirmed at",
    send_coli("/swm/signingcertupdate", ExpectString, 30),

    %%Backup 
    restore_backup(Config, ?BU_Name_1),
    remove_ta_from_node(),
    erase_flash(Config),

    ok.

%%%--------------------------------------------------------------------
%%% @doc Upgrade from AR capable up to ar capable up, soft fuse in 'HW'
%%%      and new CRL, do hardfactory reset and try to load sw
%%% @end
%%%--------------------------------------------------------------------
hardfactory_revocation_uc1(Config)->  
    ct:log("Upgrade from AR capable up to ar capable up, soft fuse in 'HW'~n"
	   "and new CRL, do hardfactory reset and try to load sw "),
    MeId = swm_test_lib:get_me_id(?NC_Session),
    InstallUp = proplists:get_value(upnewcrl, Config),
    UgPathUp = proplists:get_value(uprevcrl, Config),

    put_ta_on_node(),
    rcstprep(InstallUp),
    board_restore(),
    install_sw(),
 send_coli_2_command("/misc/authlevel disabled", "/labonly/swm/enable-anti-rollback on"),

    %%upgrade to AR UP and revoc UP
    swm_upg_crl_lib:run_upgrade(Config, ?NC_Session, UgPathUp),
    case site_config_complete([]) of
    	ok -> ok;
    	{error,String}  -> 
    	    ct:fail(String)
    end,
    %%check swm
    UPs = swm_test_lib:get_ups(?NC_Session),
    ct:pal("Ups ~p",[UPs]),
    %% Highest label after Upgrade.
    LatestLabel = swm_test_lib:get_highest_label(UPs),
    ct:pal("Revoc up ~p",[LatestLabel]),
    PrevLabel = lists:delete(LatestLabel, UPs),
    "COMMIT_COMPLETED" =  
	swm_test_lib:get_up_state(?NC_Session, MeId, LatestLabel),
    "PREPARE_COMPLETED"  =  swm_test_lib:get_up_state(?NC_Session, MeId, PrevLabel),
  
    %% workaround
    ct:pal("timer sleep 2 min workaround"),
    timer:sleep(120000),
    
    %%Confirme revocation up
    ExpectString = "New software signing certificates are included in the "
	"current UP and will be auto confirmed at",
    send_coli("/swm/signingcertupdate",ExpectString, 30 ),


    send_coli("/swm/signingcertupdate confirm"),
    %% timer:sleep(20000),
    ok = loop_nr_up(1, 100),
    wait_for_nr_of_exp_bu(1, MeId),

    %%Send Hard factory reset
    ct:log("### Send hard factory reset"),
    hardfactory_reset(),

    %%Load not revoc up and that will fail
    rcstprep(InstallUp),
    install_fail_sw("Signature check failed for software package"),
 
    %%install revoc up to clean up
    rcstprep(UgPathUp),
    install_sw(),

    remove_ta_from_node(),
    erase_flash(Config),
    ok.



%%========================================================================
%%       Internal functions
%%========================================================================
%%%--------------------------------------------------------------------
%%% Description: 
%%%--------------------------------------------------------------------
wait_for_nr_of_exp_bu(ExpNr, MeId) ->
        wait_for_nr_of_exp_bu(ExpNr, MeId, 600000).

wait_for_nr_of_exp_bu(ExpNr, _MeId, Timeout) when Timeout < 0 ->
    ct:pal("## ExpNr : ~p does not exist within time, tc will fail", [ExpNr]),
    ct:fail("Expected Nr of BUs does not exist within time.");
    
wait_for_nr_of_exp_bu(ExpNr, MeId, Timeout) ->
    NrOfBu = swm_br_lib:get_all_backups(?NC_Session, MeId),
    Nr = length(NrOfBu),
    %% ct:pal("Nr of BUs : ~p.", [Nr]),
    case Nr of
	ExpNr ->
	    ct:pal("## Nr Expected BUs exist. : ~p", [Nr]),
	    ok;
	 _ ->
	    ct:pal("## ## Nr: ~p, Expected BUs : ~p . not match", [Nr, ExpNr]),
	    timer:sleep(30000),
	    wait_for_nr_of_exp_bu(ExpNr, MeId, Timeout - 30000)
    end.
 
rcstprep(UpPath)->
    ct:log("Prep up ~p",[UpPath]),
    Hw = ct:get_config({test_nodes,1}),
    Reply =  os:cmd("rcstprep.sh " ++ "\"" ++atom_to_list(Hw) ++ "\"" ++" " ++ filename:join([UpPath,"*"])),
    ct:log("Reply from rcstprep ~p",[Reply]).

erase_flash(_Config)->
    ct:log("Erase flash"),
    CheckEraseRsp = "0000000 ffff ffff ffff ffff ffff ffff ffff ffff\n*\n0010000\n",
    [Mtd, _] = string:tokens(send_cmd("cat /proc/mtd | grep crl_seq_no_info"), "\:"),

    send_cmd("flash_erase /dev/" ++ Mtd ++ " 0 1"),
    Reply =  send_cmd("hexdump /dev/mtd13"),
  %%  ct:pal("Reply ~p", [Reply]),
    case Reply =:= CheckEraseRsp of
	true -> ok;
	false -> ct:fail("Reply from cmd hexdump /dev/mtd13 ~p~n"
			 "should be: ~p~n"
			 "take out node from pool" ,[Reply,CheckEraseRsp])
    end.
    
put_ta_on_node() -> 
 ct:log("Put ta on target dir: ~p",["/opt/rcs_ee/mounts/boot"]),
  
    send_cmd("mkdir " ++ ?TADIRTARGET),
    check_cmd("ls " ++ ?TADIRTARGET,[]),
    [] = send_cmd("mount --bind /opt/rcs_ee/mounts/boot "++ ?TADIRTARGET),
    [] = send_cmd("mount -o remount,rw " ++ ?TADIRTARGET),
    {ok,_} = rct_scp:to_target(scp, 
			       filename:join([?TADIRNOTTARGET, ?TAFILE]), 
			       filename:join([?TADIRTARGET, ?TAFILE]), 
			       60000, print),
    [] = send_cmd("umount "++ ?TADIRTARGET),
    [] = send_cmd("rm -R "++ ?TADIRTARGET),
    check_cmd("ls " ++ filename:join(["/opt/rcs_ee/mounts/boot", ?TAFILE]), "/opt/rcs_ee/mounts/boot/ta_test_file\n").

remove_ta_from_node()->
    [] = send_cmd("mkdir " ++ ?TADIRTARGET),
    [] = send_cmd("mount --bind /opt/rcs_ee/mounts/boot "++ ?TADIRTARGET),
    [] = send_cmd("mount -o remount,rw " ++ ?TADIRTARGET),
    [] = send_cmd("rm " ++ filename:join([ ?TADIRTARGET, ?TAFILE])), 
    [] = send_cmd("umount " ++ ?TADIRTARGET),
    [] = send_cmd("rm -R " ++ ?TADIRTARGET).
send_cmd(Cmd)->
    ok = rct_ssh:connect(ssh),
    ct:log("send cmd  ~p",[Cmd]),
    {ok,Reply} = ct_ssh:exec(ssh, Cmd, 5000),
    ok = ct_ssh:disconnect(ssh),
    ct:log("Reply ~p",[Reply]),
    Reply.
check_cmd(Cmd, ExpectedRsp)->
    ok = rct_ssh:connect(ssh),
    ct:log("send cmd  ~p",[Cmd]),
    {ok,Reply} = ct_ssh:exec(ssh,Cmd,5000),
    case Reply =:= ExpectedRsp of
	true ->
	    ok; 
	false ->
	    ct:fail("Command sent ~p response ~p",[Cmd,Reply])
    end.
secs_since_1970() ->
    {MSec,Sec,_} = os:timestamp(), 
    (MSec * 1000000) + Sec.

site_config_complete(_) ->
    site_config_complete(nc1, 1200).
site_config_complete(NC, Timeout) ->
    ct:log("Wait for netconf rbsConfigLevel: SITE_CONFIG_COMPLETE"),
    Time = secs_since_1970(),
    site_config_complete(NC, Time, Time + Timeout).

site_config_complete(NC, Time, Timeout) when Time < Timeout ->
    case ct_netconfc:open(NC,[{timeout, 5000},{user, "SysAdminTest"}, {password, "SysAdminTest"}]) of %%{user, "SystemAdministrator"}, {password, "SystemAdministrator"}]) of
	{ok,_} ->

	    case ct_netconfc:get(nc1,{'ManagedElement',
				      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				      [{managedElementId,[],["1"]},
				       {'NodeSupport',[],
					[{nodeSupportId,[],["1"]},
					 {'AutoProvisioning',[],
					  [{autoProvisioningId,[],["1"]}]}]}]}) of
		{error,no_such_client} ->% During startup, netconf sesssion may be closed, give it one more retry.
		    ct_netconfc:close_session(NC),
		    case get(netconf_disconnected_givit_1_more_try) of
			undefined ->
			    ct:log("netconf connection closed giving it one more retry"),
			    put(netconf_disconnected_givit_1_more_try,no),
			    timer:sleep(5000),
			    site_config_complete(NC,secs_since_1970(), Timeout);
			no ->
			    ct:log(lightred,"netconf connection closed  failed retry, giving up"),
			    {error, netconf_timeout}
		    end;
		{error,closed} ->% During startup, netconf sesssion may be closed, give it one more retry.
		    ct_netconfc:close_session(NC),
		    case get(netconf_disconnected_givit_1_more_try) of
			undefined ->
			    ct:log("netconf connection closed giving it one more retry"),
			    put(netconf_disconnected_givit_1_more_try,no),
			    timer:sleep(5000),
			    site_config_complete(NC,secs_since_1970(), Timeout);
			no ->
			    ct:log(lightred,"netconf connection closed  failed retry, giving up"),
			    {error, netconf_timeout}
		    end;
		{error,{closed}} ->% During startup, netconf sesssion may be closed, give it one more retry.
		    ct_netconfc:close_session(NC),
		    case get(netconf_disconnected_givit_1_more_try) of
			undefined ->
			    ct:log("netconf connection closed giving it one more retry"),
			    put(netconf_disconnected_givit_1_more_try,no),
			    timer:sleep(5000),
			    site_config_complete(NC,secs_since_1970(), Timeout);
			no ->
			    ct:log(lightred,"netconf connection closed, failed retry, giving up"),
			    {error, netconf_timeout}
		    end;
		{ok,[{'ManagedElement',
		      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		      [{managedElementId,[],["1"]},
		       {'NodeSupport',
			[{xmlns,"urn:com:ericsson:ecim:RmeSupport"}],
			[{nodeSupportId,[],["1"]},
			 {'AutoProvisioning',
			  [{xmlns,"urn:com:ericsson:ecim:RmeAI"}],
			  [{autoProvisioningId,[],["1"]},
			   {rbsConfigLevel,[],[SITE_CONFIG_COMPLETE]}]}]}]}]} ->

	    	    case SITE_CONFIG_COMPLETE of
			"SITE_CONFIG_COMPLETE" ->
			    ct:log("rbsConfigLevel: SITE_CONFIG_COMPLETE"),
			    ok = ct_netconfc:close_session(NC);		
			_ ->
			    ct:log(yellow,"Wrong rbsConfigLevel: ~s, Retrying in 5 seconds",[SITE_CONFIG_COMPLETE]),
			    timer:sleep(5000),
			    ok = ct_netconfc:close_session(NC),
			    site_config_complete(NC,secs_since_1970(), Timeout)
		    end;

		{ok,[{'ManagedElement',
		      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		      [{managedElementId,[],["1"]},
		       {'NodeSupport',
			[{xmlns,"urn:com:ericsson:ecim:RmeSupport"}],
			[{nodeSupportId,[],["1"]},
			 {'AutoProvisioning',
			  [{xmlns,"urn:com:ericsson:ecim:RmeAI"}],
			  [{autoProvisioningId,[],["1"]}]}]}]}]} 

		->
		    ct:log(yellow," rbsConfigLevel not set, Retrying in 5 seconds",[]),
		    timer:sleep(5000),
		    ok = ct_netconfc:close_session(NC),
		    site_config_complete(NC,secs_since_1970(), Timeout)
	    end;
	Other ->
	    ct:log(yellow,"Could not connect with netconf, Retrying in 5 seconds. Reason: ~p",[Other]),
	    timer:sleep(5000),
	    site_config_complete(NC,secs_since_1970(), Timeout)
    end;
site_config_complete(_NC,_Time,Timeout) ->
    ct:pal("in fail"),
    ReturnString = "Could not connect with netconf within " ++ integer_to_list(Timeout) ++ " seconds",
    {error,ReturnString} .
board_restore()->
ct:log("board restore"),
%%ok= aic_curl:board_restore(),

    case aic_curl:board_restore() of
    	ok ->    
    	    check_expect_from_console("boot_count = 7",300000),
    	    check_expect_from_console("Network Loader Ready:","Reset Status = SW Ordered",
    				      60000, "Detect extra restart");
    	{_, "HTTP/1.1 403 Forbidden"} -> ct:log("Probably already in NL state"), ok; 
    	Response1 ->
    	    ct:fail(Response1)
    end
. 
hardfactory_reset()->
ct:log("board restore"),
%%ok= aic_curl:board_restore(),

    case aic_curl:hard_factory_reset() of
    	ok ->    
    	    check_expect_from_console("boot_count = 7",300000),
    	    check_expect_from_console("Network Loader Ready:","Reset Status = SW Ordered",
    				      60000, "Detect extra restart");
    	{_, "HTTP/1.1 403 Forbidden"} -> ct:log("Probably already in NL state"), ok; 
    	Response1 ->
    	    ct:fail(Response1)
    end
.  
%%Check String from console log, if not try to fetch fake esi in NL state
check_expect_from_console(String, TimeOut)->
    case  ct_telnet:expect(console1, String, [{timeout,TimeOut},no_prompt_check]) of
	{ok, _} -> ok;
	_ ->  ct:fail("Didn't receive ~s",[String])
    end.
%%Check String from console log, if not try to fetch fake esi in NL state
check_expect_from_console(ExpectString, HaltString, TimeOut, ErrorReason)->
    case ct_telnet:expect(console1, [{expectstring, ExpectString}], 
			   [sequence,{halt,[{haltstring,HaltString}]},
			    {timeout,TimeOut},no_prompt_check]) of
	{ok, _} -> ok;
	{error,timeout} -> 
	    ct:fail("Didn't receive ~s",[ExpectString]);
	{error,{haltstring,[HaltString]}} ->  ct:fail("Received ~p before ExpectString ~p: ~p  ",
						      [HaltString, ExpectString, ErrorReason]);
	Unknown ->  ct:fail("Unknown reason: ~p",[Unknown])
    end.  

install_sw()->
     case aic_curl:install_sw() of
	ok -> ok;
	Response3 -> 	
	    ct:fail(Response3)
    end,
    case site_config_complete([]) of
	ok -> ok;
	{error,String} -> 
	    ct:fail(String)
    end.
install_fail_sw(ExpectString)->
     case aic_curl:install_sw() of
	ok -> ok;
	Response3 -> 	
	    ct:fail(Response3)
    end,
    check_expect_from_console(ExpectString, 300000).

take_out_node_from_pool(_Config)->
    Hw =atom_to_list(ct:get_config({test_nodes,1})),
    %% LogPath = ?config(priv_dir,Config),
    Cmd =   
	"curl https://rbs-rde-ci.rnd.ki.sw.ericsson.se/job/set_faulty_node_offline/buildWithParameters?token=nisse"++ "%2F"++ "NODE=" ++ Hw,
    ct:pal("CMD ~p",[Cmd]),
    send_curl_comand(Cmd).
    
send_curl_comand(Cmd)->
    ct:log("~s",[Cmd]),
    Response = os:cmd(Cmd),
    ct:log(re:replace(Response,"<","\\&lt;",[{return,list},global])),
    Response.
save_original_up(TftpDir, OriginalUp, _Config) ->
    CurrentDir = filename:absname(""),
    check_create_clean_dir(OriginalUp),
    Path = filename:join([CurrentDir,OriginalUp]),    
    CMD = "cp "  ++  filename:join([TftpDir, "*.zip"]) ++ " " ++ Path,
    case os:cmd(CMD) of
	[] -> ok;
	Reply -> ct:fail("Reason: ~p",[Reply])
    end,
    Path.
build_up(NC_Session, UgHook, NewPath, Steep, Revoc, TftpDir)  -> 
    {_StpName, CXS_LABEL} = swm_test_lib:get_info_to_build_up(NC_Session, UgHook),
    CurrentDir = filename:absname(""),
    OutPutCXS = CXS_LABEL++Revoc++ "_steepRev"++ Steep ++ ".zip",

    ct:pal("CXS_LABEL ~p",[CXS_LABEL]),
  
    UgPathNewCrl = filename:join([CurrentDir, NewPath]),
    ct:pal("UGPathNewCrl:~n~p~n",[UgPathNewCrl]),
    check_create_clean_dir(UgPathNewCrl),
   
    C_M_D = "chmod 777 " ++ filename:join([UgPathNewCrl ,"*"]),
    ct:pal("CMD:~n~p~n",[C_M_D]),
    os:cmd(C_M_D),
    timer:sleep(5000),
    
    CMD1 =  ?BUILDUPSCRIPT ++ "  " ++ filename:join([TftpDir, "*.zip"]) ++ " " ++ Steep ++ " " ++ Revoc ++ " " ++  OutPutCXS,
    ct:pal("CMD1:~n~p~n",[CMD1]),
    A = os:cmd(CMD1),
    ct:log("~s", [A]),

    os:cmd("mv " ++ OutPutCXS ++ " " ++ filename:join([UgPathNewCrl,"."])),
    UgPathNewCrl.    
check_create_clean_dir(UgPathNewCrl) ->    
    Reply = os:cmd("mkdir " ++ UgPathNewCrl), 
    C_M_D ="chmod 777 " ++ UgPathNewCrl, 
    os:cmd(C_M_D),
    case Reply =:= [] of
	true -> ok;
	false -> [] = os:cmd("rm -rf " ++ filename:join([UgPathNewCrl, "*"]))
    end.
send_coli_2_command(Comand1, Comand2) ->

    ok = rct_coli:connect(coli),
    {ok,Reply} = rct_coli:send(coli, Comand1),
    ct:log("Reply from coli: ~p",[Reply]),

    {ok,Reply2} = rct_coli:send(coli, Comand2),
    ct:log("Reply from coli: ~p",[Reply]),
    rct_coli:disconnect(coli),
    Reply2.




send_coli(Comand) ->  
    ok = rct_coli:connect(coli),
    {ok,Reply} = rct_coli:send(coli, Comand),
    ct:log("Reply from coli: ~p",[Reply]),
    rct_coli:disconnect(coli).   
send_coli(Comand, _ExpectRsp, 0) -> 
    ct:fail("Not expected rsp from command ~p",[Comand]);
send_coli(Comand, ExpectRsp, NumTry) -> 
    timer:sleep(5000), 
    ok = rct_coli:connect(coli),
    Reply2 =
	case  rct_coli:send(coli, Comand, ExpectRsp) of
	    {ok,Reply} -> Reply;
	    {_,_Reply} -> rct_coli:disconnect(coli),
			  send_coli(Comand, ExpectRsp, NumTry-1) 
	end,
    rct_coli:disconnect(coli),
    Reply2.

loop_nr_up(Nr, 0)->
    ct:fail("Number of up ~p expected ~p ",[length(swm_test_lib:get_ups(?NC_Session)), Nr]);
loop_nr_up(Nr, TimeSleep)->
   %% ct:log("loop_nr_up"),
    timer:sleep(1000),
    case Nr == length(swm_test_lib:get_ups(?NC_Session)) of
	true -> ct:log("#### Nr Expected UPs exist: ~p",[Nr]),
		ok;
	_Number -> loop_nr_up(Nr, TimeSleep-1)
    end.

upgradeprep(UgPathUp)->
    Hw =atom_to_list(ct:get_config({test_nodes,1})),
    [Up] = string:tokens(os:cmd("ls " ++ UgPathUp ),"\n"),
    UpgradePackage = 
	os:cmd("/afs/sunrise.ericsson.se/se/env/RCSDE/bin/upgradeprep.sh -stp "
	       ++ Hw ++ " "  ++ filename:join(UgPathUp, Up)),
    ct:log("Printout from upgradeprep.sh: ~n~p",[UpgradePackage]).

create_fail(_Config) ->
    ct:log("################### Create fail ##################"),
    MeId = swm_test_lib:get_me_id(?NC_Session),
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),

    [{host, SftpHost},{username, Username},{password, Password}] = 
	ct:get_config(sftp_server),

    SwVerion = swm_test_lib:get_sw_version(?NC_Session, MeId),
    ct:pal("Active SwVersion : ~p , before create up.",[SwVerion]),

    Uri = "sftp://"++Username++"@"++SftpHost++UGPath,
    Action = {'ManagedElement',
	      [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId, [], [MeId]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'SwM',
		  [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
		  [{swMId,[],["1"]},
		   {createUpgradePackage, [],
		    [{uri, [Uri]},
		     {password, [Password]}]
		   }]}]}]},
    Rsp =  [{'ManagedElement',
	     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	     [{managedElementId,[],["1"]},
	      {'SystemFunctions',[],
	       [{systemFunctionsId,[],["1"]},
		{'SwM',
		 [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
		 [{swMId,[],["1"]},
		  {createUpgradePackage,[],
		   [{returnValue,[],["0"]}]}]}]}]}],

    case netconf(?NC_Session, action, [Action]) of
	{ok, Rsp} -> ok;
	{_, FailReply} -> ct:fail("Recived ~p~nExpected ~p",[FailReply, Rsp])
    end,

    ok.


%%%--------------------------------------------------------------------
%%% Description: Netconf
%%%--------------------------------------------------------------------
netconf(Session, F, A) ->
    %% {ok, _} = ct_netconfc:open(Session, []),
    ct_netconfc:open(Session,[]),
    Res = apply(ct_netconfc, F, [Session | A]),
    %% ok = ct_netconfc:close_session(Session),
    ct_netconfc:close_session(Session),
    Res.
%%% ===========================================================================
%%% @doc
%%% Create backup <br/>
%%% Using netconf to trig a backup to be created. <br/>
%%% @spec create_backup(Config, BuName) -> ok
%%% @end
%%% ===========================================================================
create_backup(_Config, BuName) ->  
    MeId = swm_test_lib:get_me_id(?NC_Session),
    ActionId = swm_br_lib:get_action_id(?NC_Session, MeId),

    swm_br_lib:create_backup_successful(?NC_Session, 
					MeId, 
					BuName, 
					ActionId).
%%% ===========================================================================
%%% @doc
%%% Restore backup <br/>
%%% Using netconf to trig a restore backup . <br/>
%%% @spec restore_backup(_Config) -> ok
%%% @end
%%% ===========================================================================
restore_backup(_Config, BuName) ->
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Get BuId that belongs to BU_name.
    %%%%
    Backups = swm_br_lib:get_all_backups(?NC_Session, MeId),

    BuId = swm_br_lib:get_correct_buid(BuName, Backups), %% in a list.
    ct:pal("Selected buId: ~p, to restore.~n",[BuId]),

    ActionId = swm_br_lib:get_action_id(?NC_Session, MeId, {brmBackup, BuId}),

    swm_br_lib:restore_backup_successful(?NC_Session, 
					 MeId, 
					 BuId, 
					 ActionId, 
					 BuId).

%% 	send_coli("/swm/signingcertupdate", "New software signing certificates are included in the current UP and will be auto confirmed at:").	
    

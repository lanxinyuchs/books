%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lma_SUITE.erl %
%%% @author etxpejn
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R9A/R10A/R11A/1
%%%
%%% @doc Test suite for testing.....
%%%
%%% @end
%%%-------------------------------------------------------------------



-module(lma_SUITE).
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
%%% R2A/1      2013-10-03  etxpejn     Created
%%% R2A/2      2013-10-10  etxpejn     Added check_mom_tabels and 
%%%                                    change_fingerprint
%%% R2A/11      2013-11-25  etxpejn    Added eu
%%% R2A/12      2013-11-27  etxpejn    Added IU
%%% R2A/29      2014-03-12  etxpejn    Updated to fit the new GLMS ADPI
%%% R2A/31      2014-03-24  etxpejn    Moved common code to lmaTestLib, added new LKF.
%%% R2A/32      2014-03-26  etxpejn    Added check for PKI verification
%%% R2A/33      2014-03-31  etxpejn    Added tc restart_glms to all
%%% R2A/35      2014-04-02  etxpejn    Added tc fetch_glms_dumps to all
%%% R3A/3       2014-11-27  etxpejn    Added tc capacity_mo
%%% R3A/4       2015-02-27  etxkols    Preparation for 2 labs
%%% R3A/14      2015-03-04  etxpejn    Added tags for requirements
%%% R4A/5       2015-07-14  etxjovp    Add group definitions used by CS CI
%%% R4A/7       2015-09-03  etxpejn    Added extended_test_mom
%%% R4A/11      2015-10-12  etxjotj    OTP18: Replaced erlang:now
%%% R5A/1       2015-11-09  etxpejn    Added check for featureState on CapacityState MOs.
%%% R5A/5       2016-02-12  etxkols    Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
%%% R5A/6       2016-06-02  etxpejn    Do reinstall if emergancy_unlock tc fails
%%% R6A/1       2016-06-20  etxpejn    Cleanup of EU by removing file on disk
%%% R6A/2       2016-06-21  etxkols    GIT migration
%%% R6A/3       2016-06-29  etxarnu    Clear EU alarm
%%% R6A/4       2016-06-30  etxarnu    Reverted 'Clear EU alarm'
%%% R6A/5       2016-07-26  ekurnik    emergancy_unlock tc made more robust
%%% R7A/1       2016-09-12  etxpejn    emergancy_unlock tc made more robust
%%% R7A/2       2016-09-28  etxpejn    Added print of glms PIDs in restart_glms 
%%% R7A/3       2016-09-30  etxpejn    Changes to pgrep in restart_glms 
%%% R7A/4       2016-09-30  etxpejn    Removed unused find_pid
%%% R7A/6       2016-11-03  etxpejn    Added more netconf checks instead of CLI
%%% R7A/7       2016-11-04  etxpejn    Check alarm after state in emergancy_unlock
%%% R9A/1       2017-01-31  ekurnik    Added installKeyFile over FTPES test
%%% R9A/2       2017-02-21  ekurnik    Minor fix in FTPES test
%%% R9A/3       2017-02-28  etxpejn    Removed check_mom_tabels from all/0
%%% R9A/4       2017-03-02  ekurnik    Added setting FP in FTPES test
%%% R10A/1      2017-06-01  etxpejn    Activate EU via netconf
%%% R11A/1      2017-10-09  etxpejn    Changed GLMS time adjustment to avoid failed TC
%%%                                    in insecure cloud env
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
	 abort_transaction/1,
	 restart_glms/1,
	 emergancy_unlock/1,
	 production_unlock/1,
	 integration_unlock/1,
	 check_mom_tabels/1,
	 feature_mo/1,
	 capacity_mo/1,
	 install_lkf_from_sftp/1,
     install_lkf_from_ftpes/1,
	 restart/1,
	 fetch_glms_dumps/1,
	 extended_test_mom/1
	]).

-include_lib("common_test/include/ct.hrl").

-include("lihi.hrl").

-define(NON_EXISTING_LKF, "no_lkf.xml").
-define(FAULTY_LKF, "licensekeys_eu.xml").

-define(FTPES_SERVER_LMA_DIR, "LMA").

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
		 {rct_rs232, {console,[{connect_retries, 3}]}},                 
		 {rct_netconf, nc1},
		 {rct_coli, {coli, [manual_connect]}},
		 {rct_logging, {license, [{erlang,{["ERROR REPORT","CRASH REPORT"],
                           ["lmaLib: lmaGlms failed to save LKF file",
                            "lmaLib: lmaGlms failed to start channel"]}}]}},
		 {rct_htmllink,[]},
		 {rct_core,[]},
         ftpes_hook()
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
    ct:pal("Restart GLMS with adjusted timer so that 1 day turns into 2 sec"),
    rct_rpc:call(rpc_1, lmaGlms, adjust_glms_timer_test, [43200], 10000),
    timer:sleep(2000),
    Config.

%% @hidden
%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = [tuple()]
%% Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
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
init_per_testcase(install_lkf_from_ftpes, Config) ->
    
    ct:pal("rct_ftpes_state: ~p~n", [rct_ftpes_client:dump()]),
    ok = rct_ftpes_client:open(),

    ok = rct_ftpes_client:cd(?FTPES_SERVER_LMA_DIR),
    
    %% Create NodeCredential and activate ftpes feature
    Started = ftpes_test_lib:start_server(rpc_1),
    NewConfig = ftpes_test_lib:initialize_nc_tc(Config),
    ftpes_test_lib:enable_ftpes_tls(NewConfig),
    
    [{started, Started} | NewConfig];

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
end_per_testcase(emergancy_unlock, Config) ->
    case proplists:get_value(tc_status, Config) of
	ok ->
	    ok;
	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p", [Reason]),
	    Lma_Dir = rct_rpc:call(rpc_1, lmaLib, get_lma_dir, [], 10000),
	    rct_rpc:call(rpc_1, os, cmd, ["cd "++Lma_Dir++" ; rm eUPersistentData"], 
			 10000, print)
    end,
    ok;

end_per_testcase(install_lkf_from_ftpes, Config) ->
    
    ftpes_test_lib:disable_ftpes_tls(),
    case ?config(started, Config) of
        no -> ftpes_test_lib:stop_server(rpc_1);
        yes -> ok
    end,
    ftpes_test_lib:clean_nc_tc(Config),
    
    ok = rct_ftpes_client:close();
  
  
end_per_testcase(_TestCase, _Config) ->
    ct_netconfc:close_session(nc1),
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
    %% restart_glms, Not a interface tc, not needed in CI
     %% restart_glms_with_dumps, No dumps are generted, should it?
    [{group, lm_test}].

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
     {lm_test, [sequence],
     [
      %% check_mom_tabels,
      emergancy_unlock,
      production_unlock,
      integration_unlock,
      install_lkf_from_sftp,
      feature_mo,
      capacity_mo,
      fetch_glms_dumps,
      restart_glms,
      extended_test_mom
     ]},
     %% This suite should only be run on core MP within a sim cluster
     {sbc__cluster__dual_sim_1__1__group, [], [{group, default__group}]}
    ].

%%--------------------------------------------------------------------
%% @doc
%% This testcase Kills the glms process and verify that a new process is started.<br/>
%% @spec restart_glms(Config) -> ok
%% @end
%%--------------------------------------------------------------------
restart_glms(_Config) ->
    case os:getenv("SIM_OR_TARGET") of
	"target" ->
	    GlmsPid = find_glms_pid(),
	    ct:pal("GlmsPid: ~p", [GlmsPid]),
	    kill_glms_pid(GlmsPid, no_dumps),
	    NewGlmsPid = find_glms_pid(),
	    ct:pal("NewGlmsPid: ~p", [NewGlmsPid]),
	    case NewGlmsPid of
		GlmsPid ->
		    ct:fail("lma_app pid not restarted");
		_Else ->
		    ok
	    end;
	"sim" ->
	    ct:pal("This test case is not run ion a sim env."),
	    ok
    end.


%% restart_glms_with_dumps(_Config) ->
%%     rct_core:coredump_test(),
%%     GlmsPid = find_glms_pid(),
%%     ct:pal("GlmsPid: ~p", [GlmsPid]),
%%     kill_glms_pid(GlmsPid, dumps),
%%     NewGlmsPid = find_glms_pid(),
%%     ct:pal("NewGlmsPid: ~p", [NewGlmsPid]),
%%     case NewGlmsPid of
%% 	GlmsPid ->
%% 	    ct:fail("lma_app pid not restarted");
%% 	_Else ->
%% 	    ok
%%     end.

%%--------------------------------------------------------------------
%% @doc
%% This testcase will dump all information from GLMS and LMA server. The testcase will not 
%% check if the data is correct instead it will verify that GLMS doesn't crash or errors
%% will be written in the erlang log. <br/>
%% @spec fetch_glms_dumps(Config) -> ok
%% @end
%%--------------------------------------------------------------------
fetch_glms_dumps(_Config) ->
    ok = ?RPC_CALL(lmaDebug, glms_info, []),
    ok = ?RPC_CALL(lmaDebug, lma_info, []),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% This testcase checks that functionality of WP4530:
%% If a LKF file is installed during AI that has the hidden MOM CXC4011959
%% any extended MOMs will be loaded into COM by GMF. The test cases will fail if
%% not the DUMMY CXP is loaded on the node.<br/>
%% @spec extended_test_mom(Config) -> ok
%% @end
%%--------------------------------------------------------------------
extended_test_mom(_Config) ->
    R = ?RPC_CALL(sysEnv, releases_vsn_dir, []),
    Rest = "comte/model_file_list.cfg",
    {ok, FileData} = ?RPC_CALL(file, read_file, [filename:join([R, Rest])]),
    Options = [global, {capture, all, binary}],
    case os:getenv("SIM_OR_TARGET") of
	"sim" ->
	    ct:pal("No AI is done in the simulated env, make sure that no extended MOM is loaded"),
	    Re = ".*TESTMOM_mp.xml.*";
	"target" ->
	    ct:pal("AI is done in the target env, make sure that the extended MOM is loaded"),
	    Re = ".*TESTMOM_mp_extended.xml.*"
    end,
    {match, _} =re:run(FileData, Re, Options),
    ok.	    

%--------------------------------------------------------------------
%% @doc
%% This testcase Check that all MO clase that are aoutcreated exists.<br/>
%% @spec check_mom_tabels(Config) -> ok
%% @end
%%--------------------------------------------------------------------
check_mom_tabels(_Config) ->
    ok = rct_cli:connect(cli),
    {ok,_} = rct_cli:send(cli,"show ManagedElement=1,SystemFunctions=1","Lm=1"),
    {ok,_} = rct_cli:send(cli,"show ManagedElement=1,SystemFunctions=1,Lm=1", "KeyFileManagement=1"),
    {ok,_} = rct_cli:send(cli,"show ManagedElement=1,SystemFunctions=1,Lm=1", "AutonomousMode=1"),
    {ok,_} = rct_cli:send(cli,"show ManagedElement=1,SystemFunctions=1,Lm=1", "EmergencyUnlock=1"),
    {ok,_} = rct_cli:send(cli,"show ManagedElement=1,SystemFunctions=1,Lm=1", "IntegrationUnlock=1"),
    ok = rct_cli:disconnect(cli),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% This testcase activates emergency unlock, checks that the LM state has changed to <br/>
%% EMERGENCY_UNLOCK and that the EmergencyUnlockResetKeyRequired alarm is vissible. <br/>
%% <pre> 
%% slogan=Emergency Unlock activated once.
%% useCase=NodeUC450.N1 
%% requirementDocument=105 65-0771/00328  
%% non-requirement=
%% IWD= 
%% addTR= 
%% </pre> 
%% <pre> 
%% slogan=Emergency Unlock activated twice (or second time within seven days)
%% useCase=NodeUC450.A1
%% requirementId=105 65-0771/00328
%% non-requirement=
%% IWD= 
%% addTR= 
%% </pre> 
%% @end
%% @spec emergancy_unlock(Config) -> ok 
%% @end
%%--------------------------------------------------------------------
emergancy_unlock(_Config) ->
    ok = rct_cli:connect(cli),
    case lmaTestLib:wait_for_eu_via_netconf(activationsLeft, "0", 1) of
 	ok ->
	    ct:pal("Emergency unlock has already been activated 2 times");
	nok ->
	    %% EU test
	    ct:pal("Activate Emergency unlock"),
	    ok = lmaTestLib:activate_eu(),
	    ok = lmaTestLib:wait_for_eu_via_netconf(activationState, "ACTIVATED", ?NO_OF_TRIES),

	    %% EU LM state test
	    ct:pal("Check that the LM state has changed to EMERGENCY_UNLOCK"),
	    ok = lmaTestLib:wait_for_lm_state_via_netconf("EMERGENCY_UNLOCK", ?NO_OF_TRIES),
	    ok = lmaTestLib:wait_for_eu_via_netconf(activationsLeft, "1", 1),

	    ct:pal("Check that EU state has changed to ACTIVATED_EXPIRING when 72H remains"),
	    ok = lmaTestLib:wait_for_eu_via_netconf(activationState, "ACTIVATED_EXPIRING", 
						    ?NO_OF_TRIES_XL),

	    ct:pal("Check that EU state has changed to INACTIVE when first EU activation has "
		   "ended"),
	    ok = lmaTestLib:wait_for_eu_via_netconf(activationState, "INACTIVE", 
						    ?NO_OF_TRIES_XL),
	    ok = lmaTestLib:wait_for_eu_via_netconf(activationsLeft, "1", 1),
	    
	    %% First time EU is activated
	    %% EU larm test
	    ct:pal("Check that the 'EmergencyUnlockResetKeyRequired' alarm is visible"),
	    {_, ok} = lmaTestLib:find_mo_from_cli("FmAlarm=", ?FM_MO, 
						  ["minorType=9175048",
						   "activeSeverity=WARNING"], 
						  ?NO_OF_TRIES, ok),


	    %% Activate EU for the second time
	    ct:pal("Activate Emergency unlock for the second time"),
	    ok = lmaTestLib:activate_eu(),

	    ok = lmaTestLib:wait_for_eu_via_netconf(activationState, "ACTIVATED", 
						    ?NO_OF_TRIES_XL),
	    ok = lmaTestLib:wait_for_eu_via_netconf(activationsLeft, "0", 1),

	    ct:pal("Check that EU state has changed to ACTIVATED_EXPIRING when 72H remains"),
	    ok = lmaTestLib:wait_for_eu_via_netconf(activationState, "ACTIVATED_EXPIRING", 
						    ?NO_OF_TRIES_XL),
	    
	    ct:pal("Check that EU state has changed to EXPIRED when second EU activation has "
		   "ended"),
	    ok = lmaTestLib:wait_for_eu_via_netconf(activationState, "EXPIRED", 
						    ?NO_OF_TRIES_XL),
	    ok = lmaTestLib:wait_for_eu_via_netconf(activationsLeft, "0", 1),

	    ct:pal("Check that the 'EmergencyUnlockResetKeyRequired' alarm is visible"),
	    {_, ok} = lmaTestLib:find_mo_from_cli("FmAlarm=", ?FM_MO,
						  ["minorType=9175048",
						   "activeSeverity=MAJOR"],?NO_OF_TRIES, 
						  ok),

	    Lma_Dir = rct_rpc:call(rpc_1, lmaLib, get_lma_dir, [], 10000),
	    rct_rpc:call(rpc_1, os, cmd, ["cd "++Lma_Dir++" ; rm eUPersistentData"], 
			 10000, print)
    end,
    ok = rct_cli:disconnect(cli).

%%--------------------------------------------------------------------
%% @doc
%% This testcase checks that it is possible to activate production unlock if a LKF has not been 
%% installed at the node.<br/>
%% If a LKF har den installed if should not be possible to activate production unlock again.<br/>
%% @spec production_unlock(Config) -> ok
%% @end
%%--------------------------------------------------------------------
production_unlock(_Config) ->
    ok = rct_cli:connect(cli),
    case lmaTestLib:check_if_fingerprint_updateable() of
 	ok ->
	    ct:pal("Activate Production unlock"),
	    ok = pu(activate),
	    timer:sleep(8000*2),
	    ok = pu(expired);
	_Else ->
	    ct:pal("Not able to test activate production unlock since a LKF has already been "
		   "installed at the node")
    end,
    ct:pal("Deactivate Production unlock"),
    ok = pu(deactivate),

    ct:pal("Try to activate Production unlock again, should fail due to that LKF is installed"),
    nok = pu(activate),
    ok = rct_cli:disconnect(cli),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% This testcase checks that it is possible to activate integration unlock if a LKF has not been 
%% installed at the node.<br/>
%% If a LKF har den installed if should not be possible to activate integration unlock again.<br/>
%% <pre> 
%% slogan=Activate Integration Unlock
%% useCase=NodeUC660.N
%% requirementId=105 65-0771/00328
%% non-requirement=
%% IWD= 
%% addTR= 
%% </pre> 
%% <pre> 
%% slogan=Integration Unlock is consumed due to installed LKF
%% useCase=NODEUC660.E1
%% requirementId=105 65-0771/00328 
%% non-requirement=
%% IWD= 
%% addTR= 
%% </pre> 
%% <pre> 
%% slogan=Integration Unlock Expires after 21 days 
%% useCase=NODEUC660.A2
%% requirementId=105 65-0771/00328 
%% non-requirement=
%% IWD= 
%% addTR= 
%% </pre> 
%% <pre> 
%% slogan=Integration Unlock is consumed due to installed LKF 
%% useCase=NODEUC660.E1
%% requirementId=105 65-0771/00328
%% non-requirement=
%% IWD= 
%% addTR= 
%% </pre> 
%% @end
%% @spec integration_unlock(Config) -> ok
%% @end
%%--------------------------------------------------------------------
integration_unlock(_Config) ->
    ok = rct_cli:connect(cli),
    case lmaTestLib:check_if_fingerprint_updateable() of
	ok ->
	    ct:pal("Activate Integration unlock"),
	    rct_cli:send(cli, ?IU_MO),
	    {ok, A} = rct_cli:send(cli,"activate"),
	    true = lists:member("true", string:tokens(A, "\r\n ")),
	    rct_cli:send(cli, "top"),
	    ok = lmaTestLib:check_data_from_cli(["activationState=ACTIVATED", "activationsLeft=0"],
						?IU_MO, ?NO_OF_TRIES),
	    timer:sleep(18000*2),
	    ok = lmaTestLib:check_data_from_cli(["activationState=ACTIVATED_EXPIRING", 
						 "activationsLeft=0"],
						?IU_MO, ?NO_OF_TRIES),

	    ok = lmaTestLib:wait_for_lm_state_via_netconf("INTEGRATION_UNLOCK", ?NO_OF_TRIES),

	    ok = lmaTestLib:check_data_from_cli(["activationState=EXPIRED", 
						 "activationsLeft=0"],
						?IU_MO, ?NO_OF_TRIES),

	    ok = lmaTestLib:wait_for_lm_state_via_netconf("LOCKED", ?NO_OF_TRIES),

	    ct:pal("Try to activate Integration unlock again, should fail"),
	    rct_cli:send(cli, ?IU_MO),
	    {ok, An} = rct_cli:send(cli,"activate"),
	    %% "false" is the answer to activate
	    true = lists:member("false", string:tokens(An, "\r\n ")),
	    rct_cli:send(cli, "top"); 
	_Else ->
	    ct:pal("Try to activate Integration unlock after install LKF, should fail"),
	    rct_cli:send(cli, ?IU_MO),
	    {ok, Answer} = rct_cli:send(cli,"activate"),
	    %% "false" is the answer to activate and the IU and LM MO should not have changed
	    true = lists:member("false", string:tokens(Answer, "\r\n ")),
	    rct_cli:send(cli, "top"),

	    nok = lmaTestLib:wait_for_lm_state_via_netconf("INTEGRATION_UNLOCK", ?NO_OF_TRIES),
	    nok = lmaTestLib:check_data_from_cli(["activationState=ACTIVATED"], ?IU_MO, 
						 ?NO_OF_TRIES)
    end,
    ok = rct_cli:disconnect(cli),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% This testcase checks that it is possible to change the fingerprint from both CLI and Netconf.<br/>
%% It checks that a install LKF will fail if the fingerprint is wrong but also that it can succeed.<br/>
%% Checks are added so that featureKey MOs are created after install and that it is possible to 
%% change the feauteState for a key.<br/>
%% Finaly it also checks that it is not possible to change the fingerprint after a LKF has been installed.<br/>
%% @spec install_lkf_from_sftp(Config) -> ok
%% @end
%%--------------------------------------------------------------------
install_lkf_from_sftp(_Config) ->
    
    ok = rct_cli:connect(cli),

    case lmaTestLib:check_if_fingerprint_updateable() of
 	ok ->
	    ok = check_if_sequence_no_is_0(),
	    
	    %% LKF alarm test
	    ct:pal("Check that the 'KeyFileFault' alarm is visible"),
	    {_Mo, ok} = lmaTestLib:find_mo_from_cli("FmAlarm=", ?FM_MO, 
						    ["minorType=9175046","activeSeverity=CRITICAL"], 
						    ?NO_OF_TRIES, ok),
	    %% Fingerpint test
	    ct:pal("Changing fingerprint from CLI"),
	    lmaTestLib:changing_fingerprint_from_cli("MisspelledFingerprintCli"),

	    ct:pal("Check from CLI that the fingerprint has changed"),
	    ok = lmaTestLib:check_data_from_cli(["fingerprint=\"MisspelledFingerprintCli"], 
						?LM_MO, ?NO_OF_TRIES),
	    ct:pal("Changing fingerprint from Netconf"),
	    changing_fingerprint_from_netconf("MisspelledFingerprint_NC"),
	    ok = lmaTestLib:check_fingerprint_from_netconf(["MisspelledFingerprint_NC"]),
	    
	    %% Install LKF fails
	    ct:pal("Try to install LKF, should fail due to wrong fingerprint"),
	    install_lkf_from_netconf("1"),
	    ok = lmaTestLib:check_data_from_cli(
		   ["state=FINISHED", "result=FAILURE", 
		    "resultInfo=\"Failed to verify the key file: bad fingerprint"], 
		   ?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),

	    ct:pal("Try to install LKF, should fail due to non exitsing LKF"),
	    install_lkf_from_netconf("2", ?NON_EXISTING_LKF),
	    ok = lmaTestLib:check_data_from_cli(["state=FINISHED", "result=FAILURE", 
						 "resultInfo=\"Failed to download the key file"], 
						?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),

	    ct:pal("Try to install LKF, should fail due wrong signature"),
	    install_lkf_from_netconf("3", ?FAULTY_LKF),
	    ok = lmaTestLib:check_data_from_cli(
		   ["state=FINISHED", "result=FAILURE", 
		    "resultInfo=\"Signature verification of key file failed"], 
		   ?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),

	    ct:pal("Try to install local LKF, should fail since not supported "),
	    {ok,_} = ct_netconfc:open(nc1, []),
	    {error,
	     [_,_,_,{'error-message',_,
		     ["Request could not be performed - resource not available, "
		      "[URI protocol not supported]"]}]} = 
		ct_netconfc:action(nc1,
				   {'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				    [{managedElementId,[],["1"]},
				     {'SystemFunctions',
				      [{systemFunctionsId,[],["1"]},
				       {'Lm',
					[{lmId,[],["1"]},
					 {'KeyFileManagement',
					  [{keyFileManagementId,[],["1"]},
					   {'installKeyFile', [], 
					    [{uri, [], ["file://home/sirpa"++?LKF]}, 
					     {password, ["passord"]}]}
					  ]}]}]}]}),
		    
	    %% RefreshLicenseInventory test
	    ct:pal("Try to refresh license inventory, should fail due to no LKF availbale"),
	    ok = refresh("false"),

	    %% Fingerpint test
	    ct:pal("Changing to correct fingerprint from CLI"),
	    lmaTestLib:changing_fingerprint_from_cli(?Fingerprint),
	    ct:pal("Check from CLI that the fingerprint has changed"),
	    ok = lmaTestLib:check_data_from_cli(["fingerprint=\"" ++ ?Fingerprint], ?LM_MO, 
						?NO_OF_TRIES);
	nok ->
	    ct:pal("Not able to run tests for changing fingerprint!!")
    end,
    
    ct:pal("Install LKF"),
    ActionId = lmaTestLib:install_lkf(),
    ok = lmaTestLib:check_data_from_cli(["actionId="++ActionId, "result=SUCCESS", "state=FINISHED",
					 "resultInfo=\"Key File installed successfully"], 
					?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),
    nok = check_if_sequence_no_is_0(),

    %% Check that the alarms is gone
    ct:pal("Check that the 'KeyFileFault' alarm has ceased"),
    {_, nok} = lmaTestLib:find_mo_from_cli("FmAlarm=", ?FM_MO, ["minorType=9175046"], 
					     ?NO_OF_TRIES, nok),

    case lmaTestLib:find_mo_from_cli("FmAlarm=", ?FM_MO, ["minorType=9175048"], ?NO_OF_TRIES, ok) of
	{_, nok} ->
	    ct:pal("The 'EmergencyUnlockResetKeyRequired' alarm has ceased"),
	    
	    ct:pal("Check that the EU MO has changed to INACTIVE"),
	    ok = lmaTestLib:check_data_from_cli(["activationState=INACTIVE"], ?EU_MO, 
						?NO_OF_TRIES),
	    %% LM state test
	    ct:pal("Check that the LM state has changed"),
	    ok = lmaTestLib:wait_for_lm_state_via_netconf("NORMAL", 1);
	{_, ok} ->
	    %% The alarm is still visible, this is probably due to that it is not the 
	    %% first time this suite is exicuded after install.
	    %% If the alarm should not be visible every time the sequence number in the
	    %% LKF has to be increased.
	    ct:pal("The 'EmergencyUnlockResetKeyRequired' alarm has not ceased"),
	    
	    %% LM state test
	    ct:pal("Check that the LM state corresponds to the alarm"),
	    case lmaTestLib:check_data_from_cli(["activationState=EXPIRED","activationsLeft=0"],
						?EU_MO, ?NO_OF_TRIES) of
		ok ->
		    ok = lmaTestLib:wait_for_lm_state_via_netconf("NORMAL", 1);
		nok ->
		    ok = lmaTestLib:wait_for_lm_state_via_netconf("EMERGENCY_UNLOCK", 1)
	    end
    end,
    
    %% RefreshLicenseInventory test
    ct:pal("Test of refresh license inventory"),
    ok = refresh("true"),

    %% Fingerpint test
    ct:pal("Trying to change the fingerprint after a LKF has been installed"),
    lmaTestLib:changing_fingerprint("AnotherFingerprint"),
    
    ct:pal("Check from CLI that the fingerprint has not changed"),
    nok = lmaTestLib:check_data_from_cli(["fingerprint=\"AnotherFingerprint"], ?LM_MO, 0),

    ok = rct_cli:disconnect(cli).

%%--------------------------------------------------------------------
%% @doc
%% This testcase checks that it is possible to install LKF over FTPES.<br/>
%% Overall it contains 2 negative scenarios (wrong uri and wrong password) and a positive one.<br/>
%% @spec install_lkf_from_ftpes(Config) -> ok
%% @end
%%--------------------------------------------------------------------
install_lkf_from_ftpes(Config) ->
    
    ok = rct_cli:connect(cli),
    {ok, CurrentDir} = rct_ftpes_client:pwd(),
    FtpesHost = ftpes_test_lib:get_ftpes_test_server_address(ipv4),
    Username = ftpes_test_lib:get_ftpes_test_server_username(),
    Password = ftpes_test_lib:get_ftpes_test_server_password(),
    
    %% Set correct fingerprint
    ct:pal("Changing to correct fingerprint from CLI"),
    lmaTestLib:changing_fingerprint_from_cli(?Fingerprint),
    ct:pal("Check from CLI that the fingerprint has changed"),
    ok = lmaTestLib:check_data_from_cli(["fingerprint=\"" ++ ?Fingerprint], ?LM_MO, 
                    ?NO_OF_TRIES),
    
    ct:pal("Install LKF over FTPES with wrong path"),
    ActionId1 = lmaTestLib:install_lkf(ftpes, CurrentDir, ?NON_EXISTING_LKF, "", FtpesHost, Username, Password), 
    ok = lmaTestLib:check_data_from_cli(["actionId="++ActionId1, "state=FINISHED", "result=FAILURE", 
                     "resultInfo=\"Failed to download the key file"], 
                    ?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),
    
    ct:pal("Install LKF over FTPES with wrong password"),
    ActionId2 = lmaTestLib:install_lkf(ftpes, CurrentDir, ?LKF, "", FtpesHost, Username, "wrongpassword"), 
    ok = lmaTestLib:check_data_from_cli(["actionId="++ActionId2, "state=FINISHED", "result=FAILURE", 
                     "resultInfo=\"Failed to download the key file"], 
                    ?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),
    
    ct:pal("Install LKF over FTPES successfull"),
    ActionId3 = lmaTestLib:install_lkf(ftpes),
    ok = lmaTestLib:check_data_from_cli(["actionId="++ActionId3, "result=SUCCESS", "state=FINISHED",
                     "resultInfo=\"Key File installed successfully"], 
                    ?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL),

    ok = rct_cli:disconnect(cli),
    Config.


%%--------------------------------------------------------------------
%% @doc
%% This testcase checks that the feature MOs are created, both featureKey and featureState. <br/>
%% It also checks that it's possible to change the featureState to ACTIVATED. <br/>
%% @spec feature_mo(Config) -> ok
%% @end
%%--------------------------------------------------------------------
feature_mo(_Config) ->

    ok = rct_cli:connect(cli),

    %% FeatureKey MO
    ct:pal("Check that the FeatureKey MOs are created"),
    {_FeatureKeyMo, ok} = lmaTestLib:find_mo_from_cli("FeatureKey=CXC4010319", ?LM_MO, 
						      ["keyId=\"CXC4010319"], 
						      ?NO_OF_TRIES, ok),
    %% FeatureState MO
    ct:pal("Check that the FeatureState MOs are created"),
    ok = lmaTestLib:check_data_from_cli(["FeatureState=CXC4010319"], ?LM_MO, ?NO_OF_TRIES),
    
    %% Activate FeatureState and see that it becomes enabled
    ct:pal("Change the featureState to ACTIVATED for FeatureState=CXC4010319"),
    lmaTestLib:change_feature_state("ManagedElement=1,SystemFunctions=1,"
				    "Lm=1,FeatureState=CXC4010319", "ACTIVATED"),
    
    ct:pal("Check from CLI that the featureState has changed"),
    ok = lmaTestLib:check_data_from_cli(["featureState=ACTIVATED", "licenseState=ENABLED", 
					 "serviceState=OPERABLE"], 
					"ManagedElement=1,SystemFunctions=1,"
					"Lm=1,FeatureState=CXC4010319", ?NO_OF_TRIES),

    ct:pal("Change the featureState to DEACTIVATED for FeatureState=CXC4010319"),
    change_feature_state_from_netconf("CXC4010319", "DEACTIVATED"),

    ct:pal("Check from CLI that the featureState has changed"),
    ok = lmaTestLib:check_data_from_cli(["featureState=DEACTIVATED", "licenseState=ENABLED", 
					 "serviceState=INOPERABLE"], 
					"ManagedElement=1,SystemFunctions=1,"
					"Lm=1,FeatureState=CXC4010319", ?NO_OF_TRIES),
    case lmaTestLib:check_data_from_cli(["FeatureState=CXC4011266"], ?LM_MO, 1) of
    	ok ->
	    case os:getenv("SIM_OR_TARGET") of
		"target" ->
		    ct:pal("No checks on delete featureState and featureKey for non simulated env");
		"sim" ->
	    	    ct:pal("Try to delete featureKey and featureState MOs"),
		    State = rct_rpc:call(rpc_1, lmaGlms, get_state, [], 10000),
		    Spid = element(4, State),
		    ItcPort = element(5, State),
		    P = rct_rpc:call(rpc_1, erlang, whereis, [lmaGlms], 10000),
		    Index = <<2,0,0,0>>,
		    RequestId = <<2,0,0,0>>,

		    TablePerParmamBin =  list_to_binary("FeatureKeyPersistentData"),
		    TablePerParmam = change_bin_to_right_size(byte_size(TablePerParmamBin), 80, 
							      TablePerParmamBin),
		   
		    %% GLMS_ADPI_PERSISTENT_PARAMETER_DELETE_INDEX_REQ
		    rct_rpc:call(rpc_1, erlang, send, [P, {message, ItcPort, 
							   {Spid, dont_care, 16#19001b6, 
							    <<RequestId/binary, 
							      TablePerParmam/binary, 
							      Index/binary>>}}], 10000),
	    
		    %% GLMS_ADPI_SOFTWARE_PARAMETER_DELETE_INDEX_REQ
		    TableSoftParmamBin =  list_to_binary("FeatureKeySoftwareData"),
		    TableSoftParmam = change_bin_to_right_size(byte_size(TableSoftParmamBin), 80, 
							       TableSoftParmamBin),
		    rct_rpc:call(rpc_1, erlang, send, [P, {message, ItcPort, 
							   {Spid, dont_care, 16#19001c6,
							    <<RequestId/binary, 
							      TableSoftParmam/binary, 
							      Index/binary>>}}], 10000),

		    {FeatureKey, ok} = lmaTestLib:find_mo_from_cli("FeatureKey=CXC4011266", 
								   ?LM_MO, 
								   ["keyId=\"CXC4011266"], 
								   ?NO_OF_TRIES, ok),
		    Mo = lists:subtract(FeatureKey, "ManagedElement=1,SystemFunctions=1,Lm=1,"),
		    
		    %% GLMS_ADPI_DELETE_FEATURE_STATE_MO_REQ
		    KeyBin2 = list_to_binary("CXC4011266"),
		    Key2 = change_bin_to_right_size(byte_size(KeyBin2), 34, KeyBin2),
		    rct_rpc:call(rpc_1, erlang, send, [P, {message, ItcPort, 
							   {Spid, dont_care, 16#1900130,
							    <<Key2/binary>>}}], 10000),

		    %% GLMS_ADPI_DELETE_FEATURE_KEY_MO_REQ
		    KeyBin = list_to_binary(Mo),
		    Key = change_bin_to_right_size(byte_size(KeyBin), 34, KeyBin),
		    rct_rpc:call(rpc_1, erlang, send, [P, {message, ItcPort, 
							   {Spid, dont_care, 16#1900113,
							    <<Key/binary>>}}], 10000),
	    
		    ct:pal("Check that the FeatureKey MOs are deleted"),
		    {_, nok} = lmaTestLib:find_mo_from_cli("FeatureKey=CXC4011266", 
							   ?LM_MO, 
							   ["keyId=\"CXC4011266"], 
							   ?NO_OF_TRIES, nok),
	    
		    ct:pal("Check that the FeatureState MOs are deleted"),
		    nok = lmaTestLib:check_data_from_cli(["FeatureState=CXC4011266"], ?LM_MO, 1)
	    end;
    	nok ->
    	    ct:pal("Not possible to try to delete featureKey and featureState MOs")
    end,
    ok = rct_cli:disconnect(cli).

%%--------------------------------------------------------------------
%% @doc
%% This testcase checks that the capacity MOs are created, both capacityKey and capacityState. <br/>
%% @spec capacity_mo(Config) -> ok
%% @end
%%--------------------------------------------------------------------
capacity_mo(_Config) ->

    ok = rct_cli:connect(cli),

    %% CapacityKey MO
    ct:pal("Check that the CapacityKey MOs are created"),
    {CapacityKey, ok} = lmaTestLib:find_mo_from_cli("CapacityKey=CXC4011961", ?LM_MO, 
						    ["keyId=\"CXC4011961"], ?NO_OF_TRIES, ok),
    ct:pal("CapacityKey: ~p", [CapacityKey]),
    
    %% CapacityState MO
    ct:pal("Check that the CapacityState MOs are created"),
    ok = lmaTestLib:check_data_from_cli(["CapacityState=CXC4011961"], ?LM_MO, ?NO_OF_TRIES),
    
    %% Make sure that the licenseState is enabled
    ct:pal("Check from CLI that the licenseState is ENABLED"),
    ok = lmaTestLib:check_data_from_cli(["licenseState=ENABLED"], 
					"ManagedElement=1,SystemFunctions=1,"
					"Lm=1,CapacityState=CXC4011961", ?NO_OF_TRIES),

    ct:pal("Try to change the featureState to DEACTIVATED for CapacityState=CXC4011961"),
    change_feature_state_from_netconf(capacity, "CXC4011961", "DEACTIVATED"),

    ct:pal("Check from CLI that the featureState has NOT changed"),
    ok = lmaTestLib:check_data_from_cli(["featureState=ACTIVATED", "licenseState=ENABLED", 
					 "serviceState=OPERABLE"], 
					"ManagedElement=1,SystemFunctions=1,"
					"Lm=1,CapacityState=CXC4011961", ?NO_OF_TRIES),

    ct:pal("Change the featureState to ACTIVATED for CapacityState=CXC4011961"),
    lmaTestLib:change_feature_state("ManagedElement=1,SystemFunctions=1,"
				    "Lm=1,CapacityState=CXC4011961", "ACTIVATED"),
    
    ct:pal("Check from CLI that the featureState has changed"),
    ok = lmaTestLib:check_data_from_cli(["featureState=ACTIVATED", "licenseState=ENABLED", 
					 "serviceState=OPERABLE"], 
					"ManagedElement=1,SystemFunctions=1,"
					"Lm=1,CapacityState=CXC4011961", ?NO_OF_TRIES),

    case lmaTestLib:check_data_from_cli(["CapacityState=CXC4011961"], ?LM_MO, 1) of
    	ok ->
	    case os:getenv("SIM_OR_TARGET") of
		"target" ->
		    ct:pal("No checks on delete capacityState and capacityKey for non simulated env");
		"sim" ->
		    ct:pal("Try to delete capacityKey and capacityState MOs"),
		    State = rct_rpc:call(rpc_1, lmaGlms, get_state, [], 10000),
		    Spid = element(4, State),
		    ItcPort = element(5, State),
		    P = rct_rpc:call(rpc_1, erlang, whereis, [lmaGlms], 10000),
		    Index = <<8,0,0,0>>,
		    RequestId = <<2,0,0,0>>,
		    
		    TablePerParmamBin =  list_to_binary("CapacityKeyPersistentData"),
		    TablePerParmam = change_bin_to_right_size(byte_size(TablePerParmamBin), 80, 
							      TablePerParmamBin),
		    
		    %% GLMS_ADPI_PERSISTENT_PARAMETER_DELETE_INDEX_REQ
		    rct_rpc:call(rpc_1, erlang, send, [P, {message, ItcPort, 
							   {Spid, dont_care, 16#19001b6, 
							    <<RequestId/binary, 
							      TablePerParmam/binary, 
							      Index/binary>>}}], 10000),
		    
		    %% GLMS_ADPI_SOFTWARE_PARAMETER_DELETE_INDEX_REQ
		    TableSoftParmamBin =  list_to_binary("CapacityKeySoftwareData"),
		    TableSoftParmam = change_bin_to_right_size(byte_size(TableSoftParmamBin), 80, 
							       TableSoftParmamBin),
		    rct_rpc:call(rpc_1, erlang, send, [P, {message, ItcPort, 
							   {Spid, dont_care, 16#19001c6,
							    <<RequestId/binary, 
							      TableSoftParmam/binary, 
							      Index/binary>>}}], 10000),
		    
		    %% GLMS_ADPI_DELETE_CAPACITY_KEY_MO_REQ
		    Mo = lists:subtract(CapacityKey, "ManagedElement=1,SystemFunctions=1,Lm=1,"),
		    KeyBin = list_to_binary(Mo),
		    Key = change_bin_to_right_size(byte_size(KeyBin), 34, KeyBin),
		    rct_rpc:call(rpc_1, erlang, send, [P, {message, ItcPort, 
							   {Spid, dont_care, 16#1900134,
							    <<Key/binary>>}}], 10000),
	    
		    %% GLMS_ADPI_DELETE_CAPACITY_STATE_MO_REQ
		    KeyBin2 = list_to_binary("CXC4011961"),
		    Key2 = change_bin_to_right_size(byte_size(KeyBin2), 34, KeyBin2),
		    rct_rpc:call(rpc_1, erlang, send, [P, {message, ItcPort, 
							   {Spid, dont_care, 16#1900138,
							    <<Key2/binary>>}}], 10000),
	    
		    ct:pal("Check that the CapacityKey MOs are deleted"),
		    {_, nok} = lmaTestLib:find_mo_from_cli("CapacityKey=CXC4011961", ?LM_MO, 
							   ["keyId=\"CXC4011961"], ?NO_OF_TRIES, 
							   nok),
		    %% nok = lmaTestLib:check_data_from_cli(["CapacityKey=CXC4010720_1"],  ?LM_MO, 1),
		    
		    ct:pal("Check that the CapacityState MOs are deleted"),
		    nok = lmaTestLib:check_data_from_cli(["CapacityState=CXC4011961"], ?LM_MO, 1)
	    end;
	nok ->
    	    ct:pal("Not possible to try to delete capacityKey and capacityState MOs")
    end,
    ok = rct_cli:disconnect(cli).

%%--------------------------------------------------------------------
%% @doc
%% This testcase checks that the function lmaModel:abortTransaction restart GLMS without any <br/>
%% visible errors. <br/>
%% @spec abort_transaction(Config) -> ok
%% @end
%%--------------------------------------------------------------------
abort_transaction(_Config) ->
    GlmsPid = find_glms_pid(),
    ct:pal("GlmsPid BEFORE abortTransaction was called: ~p", [GlmsPid]),

    ?RPC_CALL(lmaModel, abortTransaction, [transid]),
    timer:sleep(3000),

    NewGlmsPid = find_glms_pid(),
    ct:pal("NewGlmsPid AFTER abortTransaction was called: ~p", [NewGlmsPid]),
    case NewGlmsPid of
	GlmsPid ->
	    ct:fail("lma_app pid not restarted");
	_Else ->
	    ok
    end.
    
%--------------------------------------------------------------------
%% @doc
%% This testcase Check that it is possible to restart the node after a LKF has been intsalled.<br/>
%% @spec restart(Config) -> ok
%% @end
%%--------------------------------------------------------------------
restart(_Config) ->
    ok = rct_cli:connect(cli),

    ct:pal("Changing the fingerprint from CLI"),
    lmaTestLib:changing_fingerprint_from_cli("MyFingerprint"),
    %% {ok, RecievedDataLM2} = rct_cli:send(cli, "show ManagedElement=1,SystemFunctions=1,Lm=1", print),
    ct:pal("Check from CLI that the fingerprint has changed"),
    %% ok = check_data_from_cli(["fingerprint=MyFingerprint"], RecievedDataLM2),

    ct:pal("Install LKF"),
    lmaTestLib:install_lkf(),
    %% {ok, RecievedData2} = rct_cli:send(cli, "show ManagedElement=1,SystemFunctions=1,Lm=1,"
    %% 				       "KeyFileManagement=1", print),
    %% ok = check_data_from_cli(["result=SUCCESS", "state=FINISHED"], RecievedData2), 

    ct:pal("Restarting node..."),
    rct_rpc:call(rpc, init, restart, [], 10000),
    wait_for_netconf_started(),

    ok = rct_cli:disconnect(cli),

    ok.

    
%%% Internal functions
%%--------------------------------------------------------------------
%% @doc 
%% Change the size of the binary so that it fits into the message <br/>
%% @end
%%--------------------------------------------------------------------
change_bin_to_right_size(Size, MaxSize, _Bin)  when Size > MaxSize ->
    ct:fail("Bin got to large");
change_bin_to_right_size(Size, MaxSize, Bin) when Size == MaxSize ->
    Bin;
change_bin_to_right_size(_Size, MaxSize, Bin) ->
    Add = <<0>>,
    NewBin = <<Bin/binary, Add/binary>>,
    change_bin_to_right_size(byte_size(NewBin), MaxSize, NewBin).



%%--------------------------------------------------------------------
%% @doc 
%% Activate or deactivate production unlock via COLI <br/>
%% @end
%%--------------------------------------------------------------------
pu(State) ->
    ok = rct_coli:connect(coli),
    {ok,Answer} =  
	case State of
	    deactivate ->
		rct_coli:send(coli,"/license/productionunlock -d");
	    _ActivateOrExired ->
		rct_coli:send(coli,"/license/productionunlock -a")
	end,
    ok = rct_coli:disconnect(coli),
    Data = string:tokens(Answer, "\""),
    Data2 = string:tokens(lists:flatten(Data), "\r\n "),
    TrueOrFalse = 
	case State of
	    activate ->
		lists:member("ACTIVATED", Data2);
	    deactivate ->
		lists:member("DEACTIVATED", Data2);
	    expired ->
		lists:member("EXPIRED", Data2)
	end,
    case TrueOrFalse of
	true ->
	    ok;
	false ->
	    nok
    end.
  
%%--------------------------------------------------------------------
%% @doc 
%% Return the GLMS process on the node<br/>
%% @end
%%-------------------------------------------------------------------- 	
find_glms_pid() ->
    case os:getenv("SIM_OR_TARGET") of
	"target" ->
	    find_glms_pid(15000, "target");
	"sim" ->
	    find_glms_pid(15000, "sim")
    end.

find_glms_pid(Timeout, _Env) when Timeout < 200 ->
    ct:pal("glms pids: ~p", [os:cmd("ps -fel | grep lma_app")]),
    ct:fail("lma_app not started within max timeout after restart.");
find_glms_pid(Timeout, "sim") -> 
    case rct_rpc:call(rpc_1, os, cmd, ["pgrep -u $USER lma_app"], 10000, print) of
	[] ->
	    timer:sleep(200),
	    find_glms_pid(Timeout - 200, "sim");
	Pid ->
	    [GlmsPid] = string:tokens(Pid, "\n"),
	    GlmsPid
    end;
find_glms_pid(Timeout, "target") ->
    ok = rct_rs232:login(console),
    case ct_telnet:cmd(console,"pgrep -f lma_app") of
    	    {badrpc,_} -> %{badrpc, timeout | nodedown}
    	    timer:sleep(200),
    	    find_glms_pid(Timeout - 200, "target");
    	[] ->	    
    	    timer:sleep(200),
    	    find_glms_pid(Timeout - 200, "target");
    	{ok, [_, GlmsPid, _]} ->
    	    {match,_} = re:run(GlmsPid,"^[0-9]+$",[]),
    	    GlmsPid;
    	_Else ->
    	    timer:sleep(200),
    	    find_glms_pid(Timeout - 200, "target")
    end.


%%--------------------------------------------------------------------
%% @doc 
%% Kill the GLMS process, either with or without dump generated.<br/>
%% @end
%%--------------------------------------------------------------------
kill_glms_pid(Pid, dumps) ->
    ct:pal("Cmd: \"kill -6 " ++ Pid ++ "\""),
    case os:getenv("SIM_OR_TARGET") of
	"sim" ->
	    os:cmd("kill -6 " ++ Pid);
	"target" ->
	    ct_telnet:cmd(console,"kill -6 " ++ Pid)
    end;
kill_glms_pid(Pid, no_dumps) ->
    ct:pal("Cmd: \"kill -9 " ++ Pid ++ "\""),
    case os:getenv("SIM_OR_TARGET") of
	"sim" ->
	    os:cmd("kill -9 " ++ Pid);
	"target" ->
	    ct_telnet:cmd(console,"kill -9 " ++ Pid)
    end.

changing_fingerprint_from_netconf(Fingerprint) ->
    {ok,_} = ct_netconfc:open(nc1, []),
    ok = ct_netconfc:edit_config(nc1, running,{'ManagedElement',
					       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
					       [{managedElementId,[],["1"]},
						{'SystemFunctions',
						 [{systemFunctionsId,[],["1"]},
						  {'Lm',
						   [{lmId,[],["1"]},
						    {fingerprint,[], [Fingerprint]}
						   ]}]}]}),
    ok = ct_netconfc:close_session(nc1).

change_feature_state_from_netconf(StateId, State) ->
    {ok,_} = ct_netconfc:open(nc1, []),
    R = ct_netconfc:edit_config(nc1, running,{'ManagedElement',
					      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
					      [{managedElementId,[],["1"]},
					       {'SystemFunctions',
						[{systemFunctionsId,[],["1"]},
						 {'Lm',
						  [{lmId,[],["1"]},
						   {'FeatureState',
						    [{featureStateId, [], [StateId]},
						     {featureState,[], [State]}
						     ]}]}]}]}),
    ok = ct_netconfc:close_session(nc1),
    R.

change_feature_state_from_netconf(capacity, StateId, State) ->
    {ok,_} = ct_netconfc:open(nc1, []),
    R = ct_netconfc:edit_config(nc1, running,{'ManagedElement',
					      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
					      [{managedElementId,[],["1"]},
					       {'SystemFunctions',
						[{systemFunctionsId,[],["1"]},
						 {'Lm',
						  [{lmId,[],["1"]},
						   {'CapacityState',
						    [{capacityStateId, [], [StateId]},
						     {featureState,[], [State]}
						     ]}]}]}]}),
    ct_netconfc:close_session(nc1),
    R.

check_if_sequence_no_is_0() ->
    rct_cli:send(cli, "top"),
    lmaTestLib:check_data_from_cli(["sequenceNumber=0"], ?KEYFILE_INFO_MO, 0).


install_lkf_from_netconf(ActionId) ->
    JenkinsNode = lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
    TEST_DIR = lmaTestLib:get_test_dir(),
    file:copy(TEST_DIR++?LKF, ?PROJ_DIR++JenkinsNode++"/"++?LKF),
    install_lkf(ActionId, ?LKF, JenkinsNode).

install_lkf_from_netconf(ActionId, LKF) ->
    JenkinsNode=lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
    TEST_DIR = lmaTestLib:get_test_dir(),
    file:copy(TEST_DIR++LKF, ?PROJ_DIR++JenkinsNode++"/"++LKF),
    install_lkf(ActionId, LKF, JenkinsNode).

install_lkf(ActionId, LKF, JenkinsNode) ->
    [{host, Host},{username, Username},{password, Password}] = ct:get_config(sftp_server),
    {ok,_} = ct_netconfc:open(nc1, []),

    {ok,[{'ManagedElement',[_],
	  [{managedElementId,[],["1"]},
	   {'SystemFunctions',[],
	    [{systemFunctionsId,[],["1"]},
	     {'Lm',[_],
	      [{lmId,[],["1"]},
	       {'KeyFileManagement',[],
		[{keyFileManagementId,[],["1"]},
		 {installKeyFile,[],
		  [{returnValue,[],[AId]}]}]}]}]}]}]} = 
	ct_netconfc:action(nc1,
			   {'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			    [{managedElementId,[],["1"]},
			     {'SystemFunctions',
			      [{systemFunctionsId,[],["1"]},
			       {'Lm',
				[{lmId,[],["1"]},
				 {'KeyFileManagement',
				  [{keyFileManagementId,[],["1"]},
				   {'installKeyFile', [], 
				    [{uri, [], ["sftp://"++Username++"@"++Host++?PROJ_DIR++JenkinsNode++"/"++LKF]}, 
				     {password, [Password]}]}
				  ]}]}]}]}),
    ok = ct_netconfc:close_session(nc1),
    B = integer_to_list(list_to_integer(ActionId) + 2),
    case AId of
	ActionId ->
	    ok;
	B ->
	    ct:pal("This node has been installed with AI"),
	    ok;
	_Else ->
	    ct:fail("The actionId doesn't seem to match, wanted actionId was ~p but got ~p", 
		    [ActionId, AId])
    end.

refresh(WantedAnswer) ->
    rct_cli:send(cli, ?LM_MO),
    {ok, Answer} = rct_cli:send(cli,"refreshLicenseInventory"),
    rct_cli:send(cli, "top"),
    case lists:member(WantedAnswer, string:tokens(Answer, "\r\n ")) of
	true ->
	    ok;
	false ->
	    nok
    end.

wait_for_netconf_started() ->
    wait_for_netconf_started(60000).

wait_for_netconf_started(Timeout) when Timeout < 500 ->
    ct:fail("Netconf not started within max timeout after restart.");

wait_for_netconf_started(Timeout) ->
    case ct_netconfc:open(nc1,[]) of
	{ok,_} ->
	    {ok,_} = ct_netconfc:get_config(nc1,running,{'ManagedElement',
							 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
							 [{managedElementId,[],["1"]}]}),
	    End = os:timestamp(), 
	    %% This info seems not to be used /jotj 20151012
	    End;
	_  ->
	    timer:sleep(250),
	    wait_for_netconf_started(Timeout - 250)
    end.


ftpes_hook() ->
    {rct_ftpes_client, [{port, 21}, {ip, ftpes_test_lib:get_ftpes_test_server_address(ipv4)},
                        {user, ftpes_test_lib:get_ftpes_test_server_username()},
                        {password, ftpes_test_lib:get_ftpes_test_server_password()},
                        {start_session_per_tc, false}]}.

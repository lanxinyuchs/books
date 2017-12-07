%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_ug_SUITE.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R4A/R6A/R8A/3
%%%
%%% @doc == Test Suite for Upgrade Mechanism, create, prepare, verify, activate and confirm. To UP is same as installed except rev is stepped in cxs-up xml. ==
%%% <br/><br/>
%%% @end

-module(swm_ug_SUITE).
-vsn('/main/R2A/R3A/R4A/R6A/R8A/3').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%%% R3A/1      2014-10-09 etxkols     Added console logging
%%% R3A/2      2015-01-27 etxivri     Update to measure times. 
%%%                                   Also handle sec boards.
%%% R3A/3      2015-01-28 etxivri     Changed resultdir.
%%% R3A/4      2015-01-28 etxivri     Update to prevent error in ct shell
%%%                                   when node restarts during activate.
%%% R4A/1      2015-04-29 etxivri     Update resultdir for R4.
%%% R4A/2      2015-05-21 etxkols     Fix for 2 labs
%%% R4A/3      2015-06-01 etxkols     Fix for 2 labs
%%% R4A/4      2015-09-30 etxmlar     now() depricated in OTP 18 changed to os:timestamp() 
%%% R5A/1      2015-10-07 etxivri     Update to be used in R5.     
%%% R8A/1      2016-10-27 etxberb     Added TC run_upgrade_bootfallback/1.
%%% R8A/3      2016-11-08 etxberb     For robustness: added call to
%%%                                   check_nc_session/1 in
%%%                                   run_upgrade_bootfallback/1.
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
	 write_times/1,
	 remove/1,
	 run_upgrade/1,
	 run_upgrade_bootfallback/1]).

%% -define(SftpHost, "10.68.200.11").
%% -define(SftpUser, "mauve").
%% -define(SftpPassword, "dilbert").
-define(NC_Session, nc1).
%% -define(CLI_Session, cli1).

-define(RESULTDIR, "/proj/rcs/measurements/upgrade/swm_ug_suite/r4/").
-define(RESULTDIR_NEW, "/proj/rcs/measurements/upgrade/swm_ug_suite/").

%% -define(RESULTDIR, "/home/etxivri/tmp/").

-define(RPC_CALL(__M, __F, __A), rct_rpc:call(rpc_1, __M, __F, __A, 10000)).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf.
%% @end
%%--------------------------------------------------------------------
suite() ->
    case check_if_vc_board() of
	"yes" -> [{ct_hooks, [{rct_htmllink,[]},
			      %% {rct_cli, {cli1,
			      %% 		 [{user, "SysAdminTest"}, 
			      %% 		  {password, "SysAdminTest"},
			      %% 		  manual_connect]}},
			      %% {cth_conn_log,[]},
			      {rct_upgrade,ug1},
			      {rct_netconf, {nc1, man_auth}},
			      {rct_rs232,console}
			     ]}];
	_->
	    [{ct_hooks, [{rct_htmllink,[]},
			 {rct_rpc, rpc_1},
			 %% {cth_conn_log,[]}
			 {rct_upgrade,ug1},
			 {rct_logging, 
			  {upgrade, 
			   [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
			 {rct_netconf,nc1},
			 {rct_rs232,console} ]}]
    end.

%% @hidden
init_per_suite(Config) ->
    ct:log("# init per suite build valid to up. ~n"
    	   "Create cxps that shall be used for UG."),
    swm_test_lib:build_valid_ug_packakage(?NC_Session),
    Config.

%% @hidden
end_per_suite(_Config) ->
    ok.

%% @hidden
init_per_testcase(run_upgrade_bootfallback = TestCase, Config) ->
    ct:print("Now executing ~p", [TestCase]),
    UPm_current_start = ?RPC_CALL(swmI, get_current_up_metadata, []),
    UPm_fallback_start = ?RPC_CALL(swmI, get_bootfallback_up_metadata, []),
    ct:pal("Current UP metadata:~n~p~nFallback UP metadata:~n~p",
	   [UPm_current_start, UPm_fallback_start]),
    case
 	(UPm_fallback_start == [] orelse
 	 is_same_UP(UPm_fallback_start, UPm_current_start))
 	of
 	false ->
 	    fallback_info(),
 	    UPm_fallback_init = UPm_fallback_start;
 	true ->
	    fallback_info(),
	    run_upgrade(Config),
	    wait_for_bfb_compl(),
	    fallback_info(),
	    UPm_current_ug = ?RPC_CALL(swmI, get_current_up_metadata, []),
	    UPm_fallback_init =
	        ?RPC_CALL(swmI, get_bootfallback_up_metadata, []),
	    ct:pal("Current UP metadata:~n~p~nFallback UP metadata:~n~p",
		   [UPm_current_ug, UPm_fallback_init]),
 	    case is_same_UP(UPm_current_start, UPm_fallback_init) of
 		true ->
 		    ok;
 		false ->
 		    Fail =
 			"Fallback UP is not same as current UP before upgrade",
 		    ct:fail(Fail)
 	    end
    end,
    [{uPm_fallback_init, UPm_fallback_init} | Config];
init_per_testcase(TestCase, Config) ->
    ct:print("Now executing ~p", [TestCase]),
    Config.

%% @hidden
end_per_testcase(_TestCase, Config) ->
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p.  \n", [Reason])
    end,
    ok.

%%%--------------------------------------------------------------------
%%% TestInfo:
%%%     tcId                  = "RBSCS-SWM-00002",    %% RBSCS-"FunctinalArea"-Unique identifier of the testcase
%%%     slogan                = "Perform Upgrade",    %% Describes what the testcase does
%%%     requirementDocument   = "1/00651-FCP1301402/FOO",
%%%     requirementRevision   = "PA193/FOO",
%%%     requirementIds        = {"111","222 "},  %% An tuple of requirmentsIDs this testcase attempts to cover. In order of relevance if any
%%%     useCase               = "NodeUC416.N""   %% NodeUC...."                             
%%%     non-requirement       = "usabillity"     %% "Negative test, robustness, Char, usabillity, stabillity or nice to have "
%%%     IWD                   = "Name of RBS-CS SWM uograde" %% Testcase to verify the API          
%%%     addTR                 = "HS1111" %% New testcase because TR "     
%%%     verificationStatement = "Partly, dataconversion not covered", %% Describes the requirement coverage
%%%     testDescription       = "Perfom an upgrade to sam sw base.",
%%%     traceGuidelines       = "Look for ERROR in /rcs/erlang/erlang.log*"
%%%--------------------------------------------------------------------



%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() ->
    %% [test_all].
    [create, prepare, verify, activate, confirm, write_times].

%%%--------------------------------------------------------------------
%%% @doc
%%% Create. <br/>
%%% @spec create(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
create(_Config) ->
    %% From_Label = rct_tlib:get_sw_version(?CLI_Session),
    From_Label = swm_test_lib:get_sw_version(?NC_Session),
    ct:pal("Active version before upgrade starts: ~p~n",[From_Label]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Create UG
    %%%%
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),
    Start1 = os:timestamp(),
    [{host, SftpHost},{username, Username},{password, Password}] = ct:get_config(sftp_server),
    swm_test_lib:ug_create_match_result(?NC_Session,
    					"SUCCESS",
    					SftpHost,
    					Username,
    					Password,
    					UGPath,
    					MeId),
    End1 = os:timestamp(),
    UgCreateTime = trunc(timer:now_diff(End1, Start1) /1000/1000),
    ct:pal("UgCreateTime: ~p~n",[UgCreateTime]),

    %% %% Add upgrade times. 
    %% %%Times shall be presented in char.
    add_new_config([{from_label, From_Label},
		    {create_time, UgCreateTime}]).

%%%--------------------------------------------------------------------
%%% @doc
%%% Prepare. <br/>
%%% @spec prepare(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
prepare(Config) ->
    MeId = swm_test_lib:get_me_id(?NC_Session),
    To_Label = get_latest_up(),

    %%%%
    %% Prepares an upgrade package,
    %% which means downloading a complete UP
    %%%%
    Start1 = os:timestamp(),
    perform_ug_action(prepare, MeId, To_Label),
    End1 = os:timestamp(),

    UgPrepareTime = trunc(timer:now_diff(End1, Start1) / 1000/1000),
    ct:pal("UgPrepareTime: ~p~n",[UgPrepareTime]),

    %% %% Add upgrade times. 
    %% %% Times shall be presented in char.
    add_new_config(Config, [{prepare_time, UgPrepareTime}]).

%%%--------------------------------------------------------------------
%%% @doc
%%% verify. <br/>
%%% @spec verify(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
verify(Config) ->
    MeId = swm_test_lib:get_me_id(?NC_Session),
    To_Label = get_latest_up(),

    %%%%
    %% Verifies an upgrade package.
    %%%%
    Start1 = os:timestamp(),
    perform_ug_action(verify, MeId, To_Label),
    End1 = os:timestamp(),

    UgVerifyTime = trunc(timer:now_diff(End1, Start1) / 1000/1000),
    ct:pal("UgVerifyTime: ~p~n",[UgVerifyTime]),

    %% %% Add upgrade times. 
    %% %% Times shall be presented in char.
    add_new_config(Config, [{verify_time, UgVerifyTime}]).

%%%--------------------------------------------------------------------
%%% @doc
%%% activate. <br/>
%%% @spec activate(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
activate(Config) ->
    MeId = swm_test_lib:get_me_id(?NC_Session),
    To_Label = get_latest_up(),

    %%%%
    %% Activates an upgrade package.
    %% During activate the node will reboot.
    %%%%
    Start1 = os:timestamp(),
    %% perform_ug_action(activate, MeId, To_Label),
    ct:pal("Perform ug action: activate", []),
    swm_test_lib:ug_action_match_result(?NC_Session,
    					"SUCCESS",
    					To_Label,
    					MeId,
    					activate,
					dummy,
					console),
    End1 = os:timestamp(),

    UgAcivateTime = trunc(timer:now_diff(End1, Start1) / 1000/1000),
    ct:pal("UgAcivateTime: ~p~n",[UgAcivateTime]),


    %% %% Add upgrade times. 
    %% %% Times shall be presented in char.
    add_new_config(Config, [{activate_time, UgAcivateTime}]).

%%%--------------------------------------------------------------------
%%% @doc
%%% confirm. <br/>
%%% @spec confirm(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
confirm(Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Get Latest UP
    %%%%
    To_Label = get_latest_up(),

    ct:pal("Perform ug confirm~n",[]),
    Start1 = os:timestamp(),
    swm_test_lib:ug_confirm(?NC_Session,
			    To_Label,
			    MeId),
    End1 = os:timestamp(),

    UgConfirmTime = trunc(timer:now_diff(End1, Start1) / 1000/1000),
    ct:pal("UgConfirmTime: ~p~n",[UgConfirmTime]),

    %% %% Add upgrade times. 
    %% %% Times shall be presented in char.
    add_new_config(Config, [{confirm_time, UgConfirmTime}]).

%%%--------------------------------------------------------------------
%%% @doc
%%% confirm. <br/>
%%% @spec write_times(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
write_times(Config) ->
    %% %% Get installed type on node.
    %% InstalledType = ct:get_config({jenkins_config, installed_type}),
    %% ct:log("NodeType: ~p", [InstalledType]),
    %% NodeType = case InstalledType of
    %% 		   grat ->
    %% 		       "GRAT";
    %% 		   wrat ->
    %% 		       "WRAT";
    %% 		   lrat ->
    %% 		       "LRAT";
    %% 		   tcu03 ->
    %% 		       "TCU03";
    %% 		   _Undef ->
    %% 		       "CS"
    %% 	       end,

    USER = os:getenv("USER"),
    BoardType = proplists:get_value(board_type,ct:get_config(ct:get_config({test_nodes,1}))),

    ProductType = rct_check_HW:check_product_on_board(),
    ProdType = case ProductType of
		   "no_match" ->
		       "_";
		   _Other ->
		       "_"++ProductType++"_"
	       end,

    SecOrUnsec = case check_if_vc_board() of
		     "yes" -> 
			 "secure";
		     _ ->
			 "unsecure"
		 end,

    %% This suiete is not runed with pre dc build, so NodeType is not needed.
    %% FileName = NodeType++"_"++BoardType++"_"++SecOrUnsec++"_swm_ug_suite.txt",
    %% FileName = USER++"_"++BoardType++"_"++SecOrUnsec++"_swm_ug_suite.txt",
    FileName = USER++ProdType++BoardType++"_"++SecOrUnsec++"_swm_ug_suite.txt",
    To_Label = get_latest_up(),

    {_Saver, SavedConfigList} = ?config(saved_config, Config),
    ct:log("SavedConfigList: ~p",[SavedConfigList]),

    From_Label = get_time(from_label, SavedConfigList),
    UgCreateTime = get_time(create_time, SavedConfigList),
    UgPrepareTime = get_time(prepare_time, SavedConfigList),
    UgVerifyTime = get_time(verify_time, SavedConfigList),
    UgAcivateTime = get_time(activate_time, SavedConfigList),
    UgConfirmTime = get_time(confirm_time, SavedConfigList),
    TotUgTime = UgCreateTime+UgPrepareTime+UgVerifyTime+UgAcivateTime+UgConfirmTime,
    Data = [httpd_util:rfc1123_date(),
	    From_Label,
	    To_Label,
	    UgCreateTime,
	    UgPrepareTime,
	    UgVerifyTime,
	    UgAcivateTime,
	    UgConfirmTime,
	    TotUgTime],
    ct:log("Data: ~p",[Data]),

    ct:pal("From_Label: ~p ~n"
	   "To_Label: ~p ~n"
	   "UgCreateTime: ~p sec ~n"
	   "UgPrepareTime: ~p sec ~n"
	   "UgVerifyTime: ~p sec~n"
	   "UgAcivateTime: ~p sec ~n"
	   "UgConfirmTime: ~p sec~n"
	   "TotUgTime: ~p sec~n",
	   [From_Label,
	    To_Label,
	    UgCreateTime,
	    UgPrepareTime,
	    UgVerifyTime,
	    UgAcivateTime,
	    UgConfirmTime,
	    TotUgTime]),

    %%%%
    %% Update upgrade measurement file
    %%%% 
    updateMeasResFile(FileName, "~p;~p;~p;~w;~w;~w;~w;~w;~w~n", 
    		      [httpd_util:rfc1123_date(),
    		       From_Label,
    		       To_Label,
    		       UgCreateTime,
    		       UgPrepareTime,
    		       UgVerifyTime,
    		       UgAcivateTime,
    		       UgConfirmTime,
		       TotUgTime]),
    ok.

%% =============================================================================
run_upgrade(Config) ->
    create(Config),
    prepare(Config),
    verify(Config),
    activate(Config),
    confirm(Config).

%%--------------------------------------------------------------------
%% @doc run_upgrade_bootfallback
%% @end

run_upgrade_bootfallback(Config) ->
    ct:pal("Config:~n~p", [Config]),
    UPm_fallback_init = proplists:get_value(uPm_fallback_init, Config),
    Instance_fallback_init = ?RPC_CALL(swmOs, get_fallback_instance, []),
    Instance_configured_init = ?RPC_CALL(swmOs, get_active_instance, []),
    true = Instance_fallback_init /= Instance_configured_init,
    ok = ?RPC_CALL(sysRhai, setbootcounter, [3]),
    do_restart_node(),
    swm_test_lib:check_nc_session(?NC_Session),
    UPm_currentAfterFallback = ?RPC_CALL(swmI, get_current_up_metadata, []),
    fallback_info(),
    ct:pal("Expected UP metadata after fallback:~n~p", [UPm_fallback_init]),
    ct:pal("UP metadata after fallback:~n~p", [UPm_currentAfterFallback]),
    ?RPC_CALL(dbg, stop_clear, []),
    true = is_same_UP(UPm_currentAfterFallback, UPm_fallback_init),
    true = is_config_ptr_reset(Instance_fallback_init),
    ok.

%% =============================================================================
wait_for_bfb_compl() ->
    wait_for_bfb_compl(?RPC_CALL(swmFallbackList,
				 is_bootfallback_complete,
				 []),
		       0).

wait_for_bfb_compl(false, Cnt) when Cnt < 600 ->
    timer:sleep(1000),
    wait_for_bfb_compl(?RPC_CALL(swmFallbackList,
				 is_bootfallback_complete,
				 []),
		       Cnt + 1);
wait_for_bfb_compl(false, _) ->
    ct:fail("Waited 600 seconds for Fallback UP to be created");
wait_for_bfb_compl(_, _) ->
    ok.

%% =============================================================================
fallback_info() ->
    Cmds = ["df -aT",
%%%	    "lvs",
%%%	    "sfdisk -l /dev/sda",
	    "ls -la /opt/rcs_ee/mounts/boot",
	    "ls -la /software",
	    "ls -la /home/sirpa/software",
	    "ls -la /rcs/swm/home1",
	    "ls -la /rcs/swm/home1/sirpa",
	    "ls -la /rcs/swm/home1/sirpa/software",
	    "ls -la /rcs/swm/home2",
	    "ls -la /rcs/swm/home2/sirpa",
	    "ls -la /rcs/swm/home2/sirpa/software",
	    "ls -la /rcs/swm/archive",
	    "ls -la /rcs/swm/archive/*/",
	    "ls -la /rcs/swm/backup/"],
    [begin
	 Res = ?RPC_CALL(os, cmd, [Cmd]),
	 ct:log("~s ->~n~s", [Cmd, Res])
     end
     || Cmd <- Cmds],
    ct:log("Fallback UP:~n~p",
	   [?RPC_CALL(swmFallbackList, get_bootfallback_up, [])]).

%% =============================================================================
is_config_ptr_reset(FbI) ->
    is_config_ptr_reset(FbI, ?RPC_CALL(swmOs, get_active_instance, []), 0).

is_config_ptr_reset(Inst, Inst, Cnt) ->
    ct:pal("Configured pointer set to fallback instance after ~p " ++
	   case Cnt of
	       1 -> "second";
	       _ -> "seconds"
	   end,
	   [Cnt]),
    true;
is_config_ptr_reset(FbI, _, Cnt) when Cnt =< 30 ->
    timer:sleep(1000),
    is_config_ptr_reset(FbI,
			?RPC_CALL(swmOs, get_active_instance, []),
			Cnt + 1);
is_config_ptr_reset(_, _, _) ->
    false.

%% =============================================================================
is_same_UP(UPm1, UPm2) ->
    ((proplists:get_value(productName, UPm1) ==
      proplists:get_value(productName, UPm2)) andalso
     (proplists:get_value(productNumber, UPm1) ==
      proplists:get_value(productNumber, UPm2)) andalso
     (proplists:get_value(productRevision, UPm1) ==
      proplists:get_value(productRevision, UPm2))).

%% =============================================================================
do_restart_node() ->
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    ?RPC_CALL(appmI, restart_piu_cold, [?MODULE]),
    swm_test_lib:wait_for_node_state(ErlNode, down),
    swm_test_lib:wait_for_node_state(ErlNode, up),
    ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% remove. <br/>
%%% @spec remove(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
remove(_Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Get Latest UP
    %%%%
    Label = get_latest_up(),

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
get_latest_up() ->
    %% [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    %% ct:pal("Label:~n~p~n",[Label]),

    UPs = swm_test_lib:get_ups(?NC_Session),
    ct:log("UPs:~n~p~n",[UPs]),

    Label = swm_test_lib:get_highest_label(UPs),
    ct:log("Label:~n~p~n",[Label]),
    Label.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
perform_ug_action(Action, MeId, To_Label) ->
    %%%%
    %% Perform ug action
    %%%%
    ct:pal("Perform ug action:~n~p~n",[Action]),
    swm_test_lib:ug_action_match_result(?NC_Session,
    					"SUCCESS",
    					To_Label,
    					MeId,
    					Action),
    ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
get_time(Action, SavedConfList) ->
    case lists:keyfind(Action, 1, SavedConfList) of
	{Action, ActionTime} ->
	    ct:log("Action: ~p, Time: ~p", [Action, ActionTime]),
	    ActionTime; 
	_Other ->
	    ct:log("Action: ~p, Time not exist: ~p", [Action, _Other]),
	    undefined
    end.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------				     
updateMeasResFile(FileName, PrintOpts, MeasData) ->
    [{_, NodeName}] = ct:get_config(test_nodes),
    MeasInfo = MeasData++[NodeName],
    ct:pal("MeasInfo: ~p",[MeasInfo]),
    %% insert a ;~w before ~n due to the field NodeName.
    CompletePrintOpts = re:replace(PrintOpts, "~n", ";~w~n", [{return, list}]),

    ResultDir = case ct:get_config({jenkins_config, branch}) of
		    undefined ->
			?RESULTDIR_NEW++"undef";
		    "R4A" ->
			?RESULTDIR;
		    Value ->
			ct:log("Branch: ~p", [Value]),
			?RESULTDIR_NEW++Value
		end,
    ct:log("ResultDir: ~p", [ResultDir]),
    
    rct_tlib:
	writeDataToFile(ResultDir, FileName, CompletePrintOpts, MeasInfo),

    ct:pal("results file: ~p ,\n path: ~p \n",[FileName, ResultDir]),

    ok.
%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------	
add_new_config(NewConfig) ->
    ct:pal("Add NewConfig: ~p",[NewConfig]),
    %% test_server:break("A"),
    {save_config, NewConfig}.

add_new_config(Config, AdditionalConfig) ->
    case ?config(saved_config, Config) of
	{_Saver, ConfigList} ->
	    add_new_config(ConfigList ++ AdditionalConfig);
	undefined ->
	    ok
    end.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------	
check_if_vc_board()->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
    Secure_board.


%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_rob_power_activate_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R5A/R7A/1
%%% 
%%% @doc == unexpected power off/on in early phase of activate.  ==
%%% <br/><br/>
%%% @end

-module(swm_rob_power_activate_SUITE).
-vsn('/main/R2A/R3A/R5A/R7A/1').

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
%%% R2A/2      2014-05-12 etxivri     Created
%%% R2A/3      2014-05-12 etxivri     Update goup_1
%%% R2A/4      2014-06-04 etxivri     Changed check for "du1 login" prompt to 
%%%                                   "login:" 
%%% R2A/5      2014-06-24 etxivri     Update due to changed reportProgress
%%% R2A/7      2014-06-26 etxivri     Added wait forntp synch
%%% R2A/8      2014-08-11 etxivri     Update due to new behaviour.
%%% R3A/1      2014-08-11 etxivri     Update group_1
%%% R3A/2      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/3      2015-05-28 etxkols     Changed rct_netconf hook format 
%%% R5A/1      2016-01-21 erarafo     Fixed deprecation warning
%%% R7A/1      2016-09-06 etxivri     Changed random to rand due to random is
%%%                                   depricated in OTP19.
%%% ----------------------------------------------------------

%-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 groups/0]).
-export([build_to_up/1,
	 random_power/1,
	 trig_power_1/1,
	 trig_power_2/1]).

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).
-define(NC_Session, nc1).
-define(CLI_Session, cli1).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{timetrap, {hours, 2}},
     {ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc_1},
		 {rct_upgrade,ug1},
		 {cth_conn_log, []},
		 {rct_power,node},
		 {rct_rs232,console},
		 {rct_core,[]},
                 {rct_logging, {upgrade, 
				[{erlang,{["ERROR REPORT",
					   "CRASH REPORT"],
					  ["swmBackup: Failed metadata read"
					  ]
					 }}]}},
		 {rct_cli, {cli1, [manual_connect]}},
		 {rct_netconf,nc1}]}].


%% @hidden
init_per_suite(Config) ->
    %% ct:log("# init per suite. ~n"
    %% 	   "Create cxps that shall be used for UG."), 
    %% swm_test_lib:build_valid_ug_packakage(?NC_Session),
    Config.

%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    swm_test_lib:wait_for_node_state(ErlNode, up),
    swm_test_lib:check_nc_session(?NC_Session),
    ct_netconfc:close_session(?NC_Session),
    Config.
%% @hidden
end_per_testcase(_TestCase, Config) ->
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
      	    ct:pal("Testcase failed due to: ~p.  \n"
    		   "Clean up! ", [Reason]),
    	    %% Remove created upgrade package.
	    swm_test_lib:check_nc_session(?NC_Session),
	    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    	    ct:pal("Label:~n~p~n",[Label]),
    	    swm_test_lib:remove_upgrade_package(?NC_Session, Label)
		
    end,
    
    ok.


%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() -> 
    [build_to_up,
     random_power,
     trig_power_1,
     trig_power_2].

groups() ->
    [{group_1,[],[build_to_up,
		  %% random_power,
		  %% %% trig_power_1,
		  trig_power_2
		 ]}
    ].

%%%--------------------------------------------------------------------
%%% @doc Create a valid up.
%%% @end
%%%--------------------------------------------------------------------
build_to_up(_Config) ->
    ct:pal("# init per suite. ~n"
	    "Create cxps that shall be used for UG."), 
    swm_test_lib:build_valid_ug_packakage(?NC_Session).


%%%--------------------------------------------------------------------
%%% @doc random power .
%%% @end
%%%--------------------------------------------------------------------
random_power(_Config) ->
    %%%%
    %% Get a random nr between 1-20, that will be used in sleep before power.
    %%%%
    Random = rand:uniform(10),
    ct:pal("TC will use Random : ~p",[Random]),
    activate_fail_due_to_power(random, Random).

%%%--------------------------------------------------------------------
%%% @doc random power .
%%% @end
%%%--------------------------------------------------------------------
trig_power_1(_Config) ->
    activate_fail_due_to_power(trig, "Database backup complete").

%%%--------------------------------------------------------------------
%%% @doc random power .
%%% @end
%%%--------------------------------------------------------------------
trig_power_2(_Config) ->
    activate_fail_due_to_power(trig, "Loading OS").
%%%--------------------------------------------------------------------
%%% @doc 
%%% @end
%%%--------------------------------------------------------------------
activate_fail_due_to_power(Flag, Value) ->
    ct:pal("UG. unexpected power before restart in activativate phase.",[]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    Label = create_prepare_verify(MeId),

    %%%%
    %% Activates an upgrade package.
    %% During activate the node will power.
    %%%%
    %% ActionId = swm_test_lib:get_action_id(?NC_Session, MeId),
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					activate, 
    					Label, 
    					MeId),

    case Flag of
	random ->
	    ct:pal("sleep : ~p, then power off/on node.",[Value]),
	    ct:sleep({seconds, Value});
	trig ->
	    wait_for_trig_value_exist(?NC_Session, MeId, Value)
	    %% wait_for_progress_info(?CLI_Session, "Mountingsoftware")
    end,
    
    %%%%
    %% Power off/on node during activate.
    %%%%
    ok = ct:pal("### power off/on!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    net_kernel:disconnect(ErlNode),
    ok = rct_power:cycle(node),
    {ok,_} = ct_telnet:expect(console, "login:", 
    			      [{timeout,60000}, no_prompt_check]),


    %%%%
    %% check node restarts
    %%%%
    swm_test_lib:wait_for_node_state(ErlNode, up),
    net_kernel:connect(ErlNode),
    ct_netconfc:close_session(?NC_Session), %% Clean up at testserver side.
    swm_test_lib:check_nc_session(?NC_Session),

    %% ct:pal("Wait for progress after power.", []),
    %% %% test_server:break("A"),

    %% case swm_test_lib:
    %% 	wait_for_progress_result(?NC_Session, MeId, ActionId, ErlNode) of
    %% 	[{"FAILURE" = Res, "activate" = ActionName, 
    %% 	  "The action was interrupted by a restart" = ProgressInfo, 
    %% 	  ResultInfo, State, _ProgReport}] ->
    %% 	    ct:pal("result:~p~n"
    %% 		   "actionName:~p~n"
    %% 		   "progressInfo:~p~n"
    %% 		   "resultInfo:~p~n"
    %% 		   "state:~p~n",[Res, 
    %% 				 ActionName, 
    %% 				 ProgressInfo, 
    %% 				 ResultInfo,
    %% 				 State]),
    %% 	    ok;
    %% 	Result ->
    %% 	    ct:pal("Fail Res: ~p",[Result]),
    %% 	    ct:fail(Result)
    %% end,

    ct:pal("Wait for up state after reboot.", []),
    swm_test_lib:wait_for_exp_up_state(?NC_Session, 
				       MeId, 
				       Label, 
				       "PREPARE_COMPLETED"),
    %%%%
    %% Remove the created up.
    %%%%
    ct:pal("Remove upgrade package: : ~p",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_Session, "SUCCESS", MeId, Label),

    swm_test_lib:wait_for_ntp_synch(rpc_1),

    ok.

%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------
create_prepare_verify(MeId) ->
    %%%%
    %% Create UG
    %%%%
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),
    %% swm_test_lib:ug_create_match_result(?NC_Session, 
    %% 					"SUCCESS", 
    %% 					?SftpHost, 
    %% 					?SftpUser, 
    %% 					?SftpPassword, 
    %% 					UGPath, 
    %% 					MeId), 

    %% OldActionId = swm_test_lib:get_action_id(?NC_Session, MeId), %% HS43611
    OldActionId = no_check,
    ok = swm_test_lib:up_create_generic(?NC_Session, 
					?SftpHost, 
					?SftpUser, 
					?SftpPassword, 
					UGPath, 
					MeId),

    case swm_test_lib:
    	wait_for_swm_progress_result(?NC_Session, MeId, OldActionId) of
    	[{"SUCCESS" = Result, 	  
    	  "createUpgradePackage" = ActionName, 
	  ProgressInfo, 
	  ResultInfo, 
    	  _State, _ProgReport}] ->
    	    ct:pal("result:~p~n"
    		   "actionName:~p~n"
    		   "progressInfo:~p~n"
    		   "resultInfo:~p~n",[Result, 
    				      ActionName, 
    				      ProgressInfo, 
    				      ResultInfo]),
    	    ok;
    	Result ->
    	    ct:pal("createUpgradePackage: ~p",[Result]),
    	    ct:fail(Result)
    end,
    

    
    %%%%
    %% Get Latest UP
    %%%%
    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    ct:pal("Label:~n~p~n",[Label]),

    %%%%
    %% Prepares an upgrade package, 
    %% which means downloading a complete UP
    %%%%
    swm_test_lib:ug_action_match_result(?NC_Session, 
					"SUCCESS",
					Label,
					MeId, 
					prepare),

    %%%%
    %% Verifies an upgrade package.
    %%%%
    swm_test_lib:ug_action_match_result(?NC_Session, 
					"SUCCESS",
					Label,
					MeId, 
					verify),
    Label.

%%%--------------------------------------------------------------------
%%% Description: 
%%%--------------------------------------------------------------------
wait_for_trig_value_exist(NC_sess, MeId, CheckVal) ->
    wait_for_trig_value_exist(NC_sess, MeId, CheckVal, 360).

wait_for_trig_value_exist(_NC_sess, _MeId, CheckVal, Timeout) when Timeout < 0 ->
    ct:fail("trig value: ~p , not found within expected time.", [CheckVal]);

wait_for_trig_value_exist(NC_sess, MeId, CheckVal, Timeout) ->
    %% {reportProgress, [{struct,"AsyncActionProgress"}], Report} = 
    %% 	swm_test_lib:get_report_progress(NC_sess, MeId),
    Report = swm_test_lib:get_report(NC_sess, MeId),
    ct:pal("Report: ~p", [Report]),

    %% %% {value, {progressInfo, _, [ProgInfo]}} = 
    %% %% 	lists:keysearch(progressInfo, 1, Report),
    %% {value, {additionalInfo, _, [AdditionalInfo]}} = 
	%% lists:keysearch(additionalInfo, 1, Report),
    %% ct:pal("ProgInfo: ~p, expect ~p .", [ProgInfo, CheckVal]),
    %% ct:pal("AdditionalInfo: ~p, expect ~p .", [AdditionalInfo, CheckVal]),
    %% Flatten = lists:flatten(AdditionalInfo),
    
    Info = [X || X <- Report, X == {additionalInfo, [], [CheckVal]}],
    case Info of
	[{additionalInfo,[],[CheckVal]}] ->
	    ct:pal("AdditionalInfo: ~p, expect ~p .", 
		   [Info, CheckVal]),
	    ok;
	[]->
	    timer:sleep(1000),
	    wait_for_trig_value_exist(NC_sess, MeId, CheckVal, 
				   Timeout - 1)
    end.

%% %%%--------------------------------------------------------------------
%% %%% Description: 
%% %%%--------------------------------------------------------------------
%% check_node_not_restarts(ErlNode) ->
%%     check_node_not_restarts(ErlNode, 150000). %% 2,5 min

%% check_node_not_restarts(_ErlNode, Timeout) when Timeout < 0 ->
%%     ct:pal("Node has not restarted unexpected. "),
%%     ok;

%% check_node_not_restarts(ErlNode, Timeout) ->
%%     case net_adm:ping(ErlNode) of
%% 	pang ->
%% 	    ct:pal("Nodedown."),
%% 	    ct:fail(" Node down unexpected");
%% 	_ ->
%% 	    timer:sleep(5000),
%% 	    check_node_not_restarts(ErlNode, Timeout-5000)
%%     end.

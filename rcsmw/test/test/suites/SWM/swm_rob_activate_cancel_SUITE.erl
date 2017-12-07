%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_rob_activate_cancel_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R7A/1
%%% 
%%% @doc == Perform cancel when activate phas is ongoing.  ==
%%% <br/><br/>
%%% @end

-module(swm_rob_activate_cancel_SUITE).
-vsn('/main/R2A/R3A/R7A/1').

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
%%% R2A/2      2014-04-10 etxivri     Created
%%% R2A/3      2014-06-04 etxivri     Minor cleanup.
%%% R3A/1      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/2      2015-05-28 etxkols     Changed rct_netconf hook format 
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
	 random_cancel/1,
	 trig_cancel_1/1,
	 trig_cancel_2/1]).

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
		 {rct_rs232,console},
		 {rct_core,[]},
                 {rct_logging, {upgrade, 
				[{erlang,{["ERROR REPORT",
					   "CRASH REPORT"],
					  []
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
     random_cancel].

groups() ->
    [{group_1,[],[build_to_up,
		  random_cancel,
		  trig_cancel_1,
		  trig_cancel_2
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
%%% @doc random reboot .
%%% @end
%%%--------------------------------------------------------------------
random_cancel(_Config) ->
    %%%%
    %% Get a random nr between 1-20, that will be used in sleep before reboot.
    %%%%
    Random = rand:uniform(20),
    ct:pal("TC will use Random : ~p",[Random]),
    cancel_during_activate(random, Random).

%%%--------------------------------------------------------------------
%%% @doc random reboot .
%%% @end
%%%--------------------------------------------------------------------
trig_cancel_1(_Config) ->
    %% activate_fail_due_to_reboot(trig, "Mounting software").
    cancel_during_activate(trig, "Software installed. Backing up data.").

%%%--------------------------------------------------------------------
%%% @doc random reboot .
%%% @end
%%%--------------------------------------------------------------------
trig_cancel_2(_Config) ->
    %% activate_fail_due_to_reboot(trig, "Mounting software").
    cancel_during_activate(trig, "Database backup complete").
%%%--------------------------------------------------------------------
%%% @doc 
%%% @end
%%%--------------------------------------------------------------------
cancel_during_activate(Flag, Value) ->
    ct:pal("UG. unexpected cancel before restart in activativate phase.",[]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    Label = create_prepare_verify(MeId),

    %%%%
    %% Activates an upgrade package.
    %%%%
    ActionId = swm_test_lib:get_action_id(?NC_Session, MeId),
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					activate, 
    					Label, 
    					MeId),

    %% wait_for_progress_info(?CLI_Session, "Mountingsoftware"),
    case Flag of
	random ->
	    ct:pal("sleep : ~p, then cancel.",[Value]),
	    ct:sleep({seconds, Value});
	trig ->
	    wait_for_trig_value_exist(?NC_Session, MeId, Value)
	    %% wait_for_progress_info(?CLI_Session, "Mountingsoftware")
    end,

    %%%%
    %% Cancel during activate.
    %%%%
    ct:pal("### cancel",[]),
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					cancel, 
    					Label, 
    					MeId),

    ct:pal("Wait for progress after cancel.", []),
    ct:pal("TC will fail due to ~nTR HS37544, Canel during activate phase does not work !! Could resulyt in beam dies (and node hangs!)."),
test_server:break("A"),

    case swm_test_lib:
    	wait_for_progress_result(?NC_Session, MeId, ActionId, ErlNode) of
    	[{"FAILURE" = Res, "activate" = ActionName, 
    	  "The action was interrupted by a restart" = ProgressInfo, 
    	  ResultInfo, State, _ProgReport}] ->
    	    ct:pal("result:~p~n"
    		   "actionName:~p~n"
    		   "progressInfo:~p~n"
    		   "resultInfo:~p~n"
    		   "state:~p~n",[Res, 
    				 ActionName, 
    				 ProgressInfo, 
    				 ResultInfo,
    				 State]),
    	    ok;
    	Result ->
    	    ct:pal("createUpgradePackage: ~p",[Result]),
    	    ct:fail(Result)
    end,


    %% %%%%
    %% %% check node restarts
    %% %%%%
    %% swm_test_lib:wait_for_node_state(ErlNode, up),
    %% net_kernel:connect(ErlNode),
    %% swm_test_lib:check_nc_session(?NC_Session),

    ct:pal("Check fallback !!.", []),
    %% test_server:break("A"),


    %% ct:pal("Wait to Check node not restarts again unexpected.",[]),
    %% check_node_not_restarts(ErlNode),

    %%%%
    %% Remove the created up.
    %%%%
    ct:pal("Remove upgrade package: : ~p",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_Session, "SUCCESS", MeId, Label),

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
    	wait_for_progress_result(?NC_Session, MeId, OldActionId) of
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


%%% %%--------------------------------------------------------------------
%% %%% Description: 
%% %%%--------------------------------------------------------------------
%% cli_connect(CLI_Session)->
%%     ok = rct_cli:connect(CLI_Session).
%% cli_disconnect(CLI_Session)->
%%     ok = rct_cli:disconnect(CLI_Session).

%% %%%--------------------------------------------------------------------
%% %%% Description: 
%% %%%--------------------------------------------------------------------
%% get_progress_info(CLI_Session) ->
%%     test_server:break("B"),
%%     cli_connect(CLI_Session),
%%     test_server:break("BB"),
%%     {ok ,RecievedData} = 
%% 	rct_cli:send(CLI_Session,
%% 		     "show ManagedElement=1,SystemFunctions=1,SwM=1"),
%%     test_server:break("BBB"),
%%     cli_disconnect(CLI_Session),
%%     ct:pal("RecievedData : ~p", [RecievedData]),

%%     Data = string:tokens(RecievedData, "=\r\n \""),
%%     ct:log("show : ~p", [Data]),
%%     Data.

%% %%%--------------------------------------------------------------------
%% %%% Description: 
%% %%%--------------------------------------------------------------------
%% wait_for_progress_info(CLI_Session, CheckVal) ->
%%     wait_for_progress_info(CLI_Session, CheckVal, 360).

%% wait_for_progress_info(_CLI_Session, _CheckVal, Timeout) when Timeout < 0 ->
%%     ct:fail(" prepare not runed within expected time.");
    
%% wait_for_progress_info(CLI_Session, CheckVal, Timeout) ->
%%     Data = get_progress_info(CLI_Session),
%%     case re:run(Data, CheckVal) of
%% 	{match, _} ->
%% 	    ok;
%% 	 nomatch ->
%% 	    timer:sleep(1000),
%% 	    wait_for_progress_info(CLI_Session, CheckVal, 
%% 				   Timeout - 1)
%%     end.

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

%%% %%--------------------------------------------------------------------
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

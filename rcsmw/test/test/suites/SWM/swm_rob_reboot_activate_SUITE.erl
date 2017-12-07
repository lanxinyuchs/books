%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_rob_reboot_activate_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R5A/R7A/1
%%%
%%% @doc == unexpected reboot in early phase of activate.  ==
%%% <br/><br/>
%%% @end

-module(swm_rob_reboot_activate_SUITE).
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
%%% R2A/2      2014-04-10 etxivri     Created
%%% R2A/7      2014-06-04 etxivri     Changed check for "du1 login" prompt to
%%%                                   "login:"
%%% R2A/8      2014-06-24 etxivri     Update due to changed reportProgress
%%% R2A/10     2014-06-26 etxivri     Some minor cleanup.
%%% R2A/11     2014-06-26 etxivri     Added wait for ntp sync
%%% R3A/1      2014-10-23 etxivri     Update what to be runed in group_1.
%%% R3A/2      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/3      2015-05-29 etxkols     Changed rct_netconf hook format
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
	 random_reboot/1,
	 trig_reboot_1/1,
	 trig_reboot_2/1,
	 trig_reboot_3/1]).

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
     random_reboot,
     trig_reboot_1].

groups() ->
    [{group_1,[],[build_to_up,
    		  %% random_reboot,
    		  %% %% trig_reboot_1,
    		  %% %% trig_reboot_2,
    		  trig_reboot_3
    		 ]}
    ].
   %% [{group_1,[],[build_to_up,
   %% 		  random_reboot
   %% 		 ]}
   %%  ].
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
random_reboot(_Config) ->
    %%%%
    %% Get a random nr between 1-20, that will be used in sleep before reboot.
    %%%%
    Random = rand:uniform(10),
    ct:pal("TC will use Random : ~p",[Random]),
    activate_fail_due_to_reboot(random, Random).

%%%--------------------------------------------------------------------
%%% @doc random reboot .
%%% @end
%%%--------------------------------------------------------------------
trig_reboot_1(_Config) ->
    activate_fail_due_to_reboot(trig, "Database backup complete").

%%%--------------------------------------------------------------------
%%% @doc random reboot .
%%% @end
%%%--------------------------------------------------------------------
trig_reboot_2(_Config) ->
    %% activate_fail_due_to_reboot(trig, "CS verifyUpgrade complete").
    activate_fail_due_to_reboot(trig, "Loading OS").

%%%--------------------------------------------------------------------
%%% @doc  .
%%% @end
%%%--------------------------------------------------------------------
trig_reboot_3(_Config) ->
    activate_fail_due_to_reboot(percentage, 20).

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
activate_fail_due_to_reboot(Flag, Value) ->
    ct:pal("UG. unexpected reboot before restart in activativate phase.",[]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    Label = create_prepare_verify(MeId),

    %%%%
    %% Activates an upgrade package.
    %% During activate the node will reboot.
    %%%%
    ActionId = swm_test_lib:get_action_id(?NC_Session, MeId),
    ok = swm_test_lib:up_action_generic(?NC_Session,
    					activate,
    					Label,
    					MeId),

    %% wait_for_progress_info(?CLI_Session, "Mountingsoftware"),
    case Flag of
	random ->
	    ct:pal("sleep : ~p, then reboot node.",[Value]),
	    ct:sleep({seconds, Value});
	trig ->
	    wait_for_trig_value_exist(?NC_Session, MeId, Value);
	percentage ->
	    ct:pal("Wait for progress precentage: ~p ",[Value]),
	    wait_for_percentage_to_be_runed(?NC_Session, MeId,
					    Value, Label)
    end,

    %%%%
    %% Reboot node during prepare.
    %%%%
    ct:pal("### reboot!",[]),
    ok = rct_rs232:login(console),
    net_kernel:disconnect(ErlNode),
    ok = ct_telnet:send(console, "reboot"),
    {ok,_} = ct_telnet:expect(console, "login:",
    			      [{timeout,60000}, no_prompt_check]),
    %%%%
    %% check node restarts
    %%%%
    swm_test_lib:wait_for_node_state(ErlNode, up),
    net_kernel:connect(ErlNode),
    ct_netconfc:close_session(?NC_Session), %% Clean up at testserver side.
    swm_test_lib:check_nc_session(?NC_Session),

    ct:pal("Wait for progress after reboot.", []),
    %% test_server:break("A"),

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
    	    ct:pal("Fail Res: ~p",[Result]),
    	    ct:fail(Result)
    end,

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
    Report = swm_test_lib:get_report(NC_sess, MeId),
    ct:pal("Report: ~p", [Report]),

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

%%%--------------------------------------------------------------------
%%% Description:
%%%--------------------------------------------------------------------
get_progress_percentage(NC_Session, MeId, UP_Label) ->
    ReportProgress =
	swm_test_lib:get_up_report_progress(NC_Session, MeId, UP_Label),
    %% ct:pal("ReportProgress: ~p", [ReportProgress]),

    {progressPercentage,[],[Percentage]} =
	lists:keyfind(progressPercentage, 1, ReportProgress),
    ct:pal("Percentage: ~p", [Percentage]),
    Percentage.

%%%--------------------------------------------------------------------
%%% Description:
%%%--------------------------------------------------------------------
wait_for_percentage_to_be_runed(NC_Session, MeId, CheckVal, UP_Label) ->
    wait_for_percentage_to_be_runed(NC_Session, MeId, CheckVal, UP_Label, 360).

wait_for_percentage_to_be_runed(_NC_Session, _MeId, _CheckVal, _UP_Label, Timeout)
  when Timeout < 0 ->
    ct:fail(" prepare not runed within expected time.");

wait_for_percentage_to_be_runed(NC_Session, MeId, CheckVal, UP_Label, Timeout) ->
    Sec = get_progress_percentage(NC_Session, MeId, UP_Label),
    Percentage = list_to_integer(Sec),
    case Percentage of
	Val when Val >= CheckVal ->
	    ct:pal("## progressPercentage: ~p",
		   [Percentage]),
	    ok;
	 _ ->
	    timer:sleep(5000),
	    wait_for_percentage_to_be_runed(NC_Session, MeId, CheckVal, UP_Label,
					 Timeout - 5)
    end.

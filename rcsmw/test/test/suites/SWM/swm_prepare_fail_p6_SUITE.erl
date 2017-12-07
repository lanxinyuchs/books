%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_prepare_fail_p6_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R11A/1
%%% 
%%% @doc == Test Suite for Upgrade Mechanism, prepare fails due to node restart unexpected.  ==
%%% <br/><br/>
%%% @end

-module(swm_prepare_fail_p6_SUITE).
-vsn('/main/R2A/R3A/R11A/1').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% R2A/2      2014-01-29 etxivri     Created
%%% R2A/3      2014-01-31 etxivri     Cleanup SUITE,use of test_swm_lib instead.
%%% R2A/4      2014-03-25 etxivri     Update to correct behaviour.
%%% R2A/6      2014-06-04 etxivri     Changed check for "du1 login" prompt to 
%%%                                   "login:" 
%%% R2A/7      2014-06-12 etxivri     Update due to swm changes of reportprogr.
%%% R2A/8      2014-06-19 etxivri     Update to check resultinfo
%%% R2A/9      2014-06-25 etxivri     Add delete nc session to clean up on 
%%%                                   testserver side
%%% R2A/10     2014-08-05 etxivri     Update due to new behaviour,
%%%                                   unexpected reboot clear ug reportProgress
%%% R2A/11     2014-08-11 etxivri     Removed a break.
%%% R3A/1      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/2      2015-05-28 etxkols     Changed rct_netconf hook format 
%%%                                   
%%% ----------------------------------------------------------

%-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).
-export([prepare_fail_due_to_reboot/1]).

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
					  [
					   %% "swmServer: throw eexist"
					  ]
					 }}]}},
		 {rct_cli, {cli1, [manual_connect]}},
		 {rct_netconf,nc1}]}].


%% @hidden
init_per_suite(Config) ->
    ct:log("# init per suite. ~n"
    	   "Create cxps that shall be used for UG."), 
    swm_test_lib:build_valid_ug_packakage(?NC_Session),
    Config.

%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
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
    [prepare_fail_due_to_reboot].

%%%--------------------------------------------------------------------
%%% @doc 
%%% During prepare the node will reboot unexpected.
%%% - Reboot node when progressPercentage has passed 50%.
%%% <br/><br/>
%%% @spec prepare_fail_due_to_reboot(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
prepare_fail_due_to_reboot(_Config) ->
    ct:pal("UG prepare fails due to node reboots unexpected.",[]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Create UG
    %%%%
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),
    swm_test_lib:ug_create_match_result(?NC_Session, 
					"SUCCESS", 
					?SftpHost, 
					?SftpUser, 
					?SftpPassword, 
					UGPath, 
					MeId),     

    %%%%
    %% Get Latest UP
    %%%%
    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    ct:pal("Label:~n~p~n",[Label]),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    %%%%
    %% Prepares an upgrade package, 
    %% which means downloading a complete UP
    %%%%
    %% ActionId = swm_test_lib:get_action_id(?NC_Session, MeId),
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					prepare, 
    					Label, 
    					MeId),

    %% wait_for_prepare_to_be_runed(?CLI_Session, 50),
    %% wait_for_prepare_to_be_runed(?NC_Session, MeId, 50, Label),
    swm_test_lib:wait_for_exp_up_state(?NC_Session, 
				       MeId, 
				       Label,
				       "PREPARE_IN_PROGRESS"),
    
    

    %%%%
    %% Reboot node during prepare.
    %%%%
    ct:pal("### reboot!",[]),
    %% {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    ok = rct_rs232:login(console),
    net_kernel:disconnect(ErlNode),
    ok = ct_telnet:send(console, "cup --reboot"),
    {ok,_} = ct_telnet:expect(console, "login:", 
    			      [{timeout,60000}, no_prompt_check]),
    %%%%
    %% check node restarts
    %%%%
    ct_netconfc:close_session(?NC_Session), %% Clean up at testserver side.
    swm_test_lib:wait_for_node_state(ErlNode, up),
    net_kernel:connect(ErlNode),
    swm_test_lib:check_nc_session(?NC_Session),

    ct:pal("Wait for progress after reboot.", []),

    %% %% Update due to new behaviour.
    %% case swm_test_lib:
    %% 	wait_for_progress_result(?NC_Session, MeId, ActionId, ErlNode) of
    %% 	[{"FAILURE" = Res, 
    %% 	  "prepare" = ActionName, 
    %% 	  ProgressInfo, 
    %% 	  "The action was interrupted by a restart" = ResultInfo, 
    %% 	  State, _ProgReport}] ->
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
    %% 	    ct:pal("prepare: ~s",[Result]),
    %% 	    ct:fail(Result)
    %% end,

    %%% This is quick check that UP state is expected.
    swm_test_lib:wait_for_exp_up_state(?NC_Session, 
				       MeId, 
				       Label, 
				       "INITIALIZED"),
    
    

    %%%%
    %% Remove the created up.
    %%%%
    ct:pal("Remove upgrade package:  ~s",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_Session, "SUCCESS", MeId, Label),

    ok.

%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------


%%% %%--------------------------------------------------------------------
%% %%% Description: 
%% %%%--------------------------------------------------------------------
%% get_progress_percentage(NC_Session, MeId, UP_Label) ->
%%     ReportProgress =
%% 	swm_test_lib:get_up_report_progress(NC_Session, MeId, UP_Label),
%%     %% ct:pal("ReportProgress: ~p", [ReportProgress]),

%%     {progressPercentage,[],[Percentage]} = 
%% 	lists:keyfind(progressPercentage, 1, ReportProgress),
%%     ct:pal("Percentage: ~p", [Percentage]),
%%     Percentage.

%% %%%--------------------------------------------------------------------
%% %%% Description: 
%% %%%--------------------------------------------------------------------
%% wait_for_prepare_to_be_runed(NC_Session, MeId, CheckVal, UP_Label) ->
%%     wait_for_prepare_to_be_runed(NC_Session, MeId, CheckVal, UP_Label, 360).

%% wait_for_prepare_to_be_runed(_NC_Session, _MeId, _CheckVal, _UP_Label, Timeout) 
%%   when Timeout < 0 ->
%%     ct:fail(" prepare not runed within expected time.");
    
%% wait_for_prepare_to_be_runed(NC_Session, MeId, CheckVal, UP_Label, Timeout) ->
%%     Sec = get_progress_percentage(NC_Session, MeId, UP_Label),
%%     Percentage = list_to_integer(Sec),
%%     case Percentage of
%% 	Val when Val >= CheckVal ->
%% 	    ct:pal("## progressPercentage: ~p", 
%% 		   [Percentage]),
%% 	    ok;
%% 	 _ ->
%% 	    timer:sleep(5000),
%% 	    wait_for_prepare_to_be_runed(NC_Session, MeId, CheckVal, UP_Label, 
%% 					 Timeout - 5)
%%     end.

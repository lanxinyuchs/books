%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_activate_fail_a19_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R8A/3
%%% 
%%% @doc == Test Suite for Upgrade Mechanism, perform cancel during activate phase.  ==
%%% <br/><br/>
%%% @end

-module(swm_activate_fail_a19_SUITE).
-vsn('/main/R2A/R3A/R4A/R8A/3').

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
%%% R2A/2      2014-03-04 etxivri     Created
%%% R2A/2      2014-03-04 etxivri     Changed to perfom cancel.
%%% R2A/4      2014-03-07 etxivri     Clean Up in TC.
%%% R2A/5      2014-04-22 etxivri     Perform cancel when Loading OS is in 
%%%                                   additionaInfo, due to reboot upgrade 
%%%                                   string not exist any more.
%%% R2A/5      2014-04-29 etxivri     Update to new behaviour. Cancel shall be ignored.
%%% R2A/7      2014-05-06 etxivri     Minor updates.
%%% R2A/8      2014-05-26 etxivri     Use reboot -f to trig fallback.
%%% R2A/9      2014-06-04 etxivri     Changed check for "du1 login" prompt to 
%%%                                   "login:"
%%% R2A/10     2014-07-04 etxivri     Minor update to prevent ERROR REPORT in 
%%%                                   ct shell when node restart.
%%% R2A/11     2014-08-15 etxivri     Update to trig cancel.
%%% R2A/12     2014-08-19 etxivri     Use cancel to trig fallback.
%%% R3A/1      2015-01-30 etxivri     Update to remove error in ct-shell.
%%% R3A/2      2015-05-08 etxivri     Changed due to new beahviour.
%%% R3A/3      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/4      2015-05-28 etxkols     Changed rct_netconf hook format 
%%% R3A/5      2015-06-17 etxivri     Make it more robust.
%%% R4A/1      2015-09-23 etxivri     Make cancel cause restart more robust.
%%% R4A/2      2015-09-25 etxivri     Update tc to be more robust.
%%% R4A/3      2015-10-30 etxivri     Removed cancel_cause_restart due to it
%%%                                   difficult to trig cancel that actual is 
%%%                                   doing cancel, Then the results is activate
%%%                                   is successful.
%%% R8A/1      2015-12-02 etxivri     Make it more robust.
%%% R8A/2      2015-12-06 etxivri     Make it more robust.
%%% R8A/2      2017-01-09 etxivri     Make it more robust.
%%% ----------------------------------------------------------

%-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).
-export([create_prep_ver/1,
	 cancel_without_restart/1,
	 cancel_cause_restart/1,
	 cancel_succ_activate/1,
	 remove_up/1
	]).

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
		 {rct_rs232,console},
		 {cth_conn_log, []},
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
    [create_prep_ver,
     cancel_without_restart,
     %% cancel_cause_restart,
     cancel_succ_activate,
     remove_up
    ].

%%%--------------------------------------------------------------------
%%% @doc 
%%% Create, prepare and verify. <br/><br/>
%%% @spec create_prep_ver(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
create_prep_ver(_Config) ->
    ct:pal("UG, Create, prepare and verify",[]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    FromUP = swm_test_lib:get_sw_version(?NC_Session, MeId),
    ct:pal("FromUP: ~p",[FromUP]),

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

    ok.

%%%--------------------------------------------------------------------
%%% @doc 
%%% Cancel during activate will not cause node restart. <br/><br/>
%%% @spec cancel_without_restart(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
cancel_without_restart(_Config) ->
    ct:pal("# Test cancel without restart. "),

    MeId = swm_test_lib:get_me_id(?NC_Session),
    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					activate, 
    					Label, 
    					MeId),

    wait_for_trig_value_exist(?NC_Session, MeId, 
			      "Last chance to cancel without restart"),
    net_kernel:disconnect(ErlNode),
    ct:pal("1. Cancel without restart",[]),

    cancel(Label, MeId),
    %% ok = swm_test_lib:up_action_generic(?NC_Session, 
    %% 					cancel, 
    %% 					Label, 
    %% 					MeId),

    swm_test_lib:
    	wait_for_expecting_state(?NC_Session, MeId, no_check, "CANCELLED"),

    {ok, _} = ct_netconfc:open(?NC_Session, []),
    ReportProgA = swm_test_lib:
    	get_up_reportprogress(?NC_Session, MeId, Label),
    ct_netconfc:close_session(?NC_Session),
    ct:pal("# ReportProgA: ~p ",[ReportProgA]),
    {progressInfo,[],["Last chance to cancel without restart"]} =
    	lists:keyfind(progressInfo, 1, ReportProgA),
 
    ct:pal("Check node doeas NOT restart."),
    {error,timeout} = ct_telnet:expect(console, "Ericsson Version:",
    			 [{timeout,60000}, no_prompt_check]),

    ok.

%%%--------------------------------------------------------------------
%%% @doc 
%%% Cancel during activate will cause node restart. <br/><br/>
%%% @spec cancel_cause_restart(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
cancel_cause_restart(_Config) ->
    ct:pal("## Test cancel cause restart. "),

    MeId = swm_test_lib:get_me_id(?NC_Session),
    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					activate, 
    					Label, 
    					MeId),

    wait_for_trig_value_exist(?NC_Session, MeId, 
			      "Cancel will now cause a restart"),
    ct:pal("2. Cancel will now cause a restart."),
    timer:sleep(1000),
    cancel(Label, MeId),
    %% ok = swm_test_lib:up_action_generic(?NC_Session, 
    %% 					cancel, 
    %% 					Label, 
    %% 					MeId),

    {ok, _} = ct_telnet:expect(console, "Ericsson Version:",
    			 [{timeout,60000}, no_prompt_check]),

    swm_test_lib:check_nc_session(?NC_Session),

    [{"FAILURE", 
      "activate", 
      "The action was interrupted by a restart", 
      _D, _E, _ProgReport}] =
    	swm_test_lib:wait_for_progress_result(?NC_Session, 
					      MeId, 
					      no_check, 
					      ErlNode),
    ok.

%%%--------------------------------------------------------------------
%%% @doc 
%%% Cancel after a completed activate will trig fallback. <br/><br/>
%%% @spec cancel_succ_activate(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
cancel_succ_activate(_Config) ->
    ct:pal("### Test cancel after activate completed successfuly. "),

    MeId = swm_test_lib:get_me_id(?NC_Session),
    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    %% swm_test_lib:ug_action_match_result(?NC_Session, 
    %% 					"SUCCESS",
    %% 					Label,
    %% 					MeId, 
    %% 					activate),
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					activate, 
    					Label, 
    					MeId),
    net_kernel:disconnect(ErlNode),
    {ok,_} = ct_telnet:expect(console, "Ericsson Version:",
    			      [{timeout,300000}, no_prompt_check]),
    ct_telnet:expect(console, "login:", 
    		     [{timeout,90000}, no_prompt_check]),
    timer:sleep(5000),
    swm_test_lib:check_nc_session(?NC_Session),
    [{"SUCCESS", 
      "activate", 
      _C, 
      _D, _E, _ProgReport}] =
    	swm_test_lib:wait_for_progress_result(?NC_Session, 
					      MeId, 
					      no_check, 
					      ErlNode),
    swm_test_lib:wait_for_action_capable(?NC_Session, MeId, "CAPABLE"),
    timer:sleep(5000),
    ct:pal("Wait for up state after reboot.", []),
    swm_test_lib:wait_for_exp_up_state(?NC_Session, 
				       MeId, 
				       Label, 
				       "WAITING_FOR_COMMIT"),
    timer:sleep(15000),
    
    %% test_server:break("break"),

    ct:pal("3. Cancel after successful activate will trig fallback.",[]),
    cancel(Label, MeId),

    ct:pal("Wait for node restart."),
    {ok,_} = ct_telnet:expect(console, "Ericsson Version:",
    			      [{timeout,40000}, no_prompt_check]),
    ct_telnet:expect(console, "login:", 
    		     [{timeout,90000}, no_prompt_check]),
    timer:sleep(5000),
    swm_test_lib:check_nc_session(?NC_Session),

    [{"FAILURE", 
      "activate", 
      "The action was interrupted by a restart", 
      _D1, _E1, _ProgReport1}] =
    	swm_test_lib:wait_for_progress_result(?NC_Session, 
					      MeId, 
					      no_check, 
					      ErlNode),

    EndFromUP = swm_test_lib:get_sw_version(?NC_Session, MeId),
    ct:pal("EndFromUP: ~p", [EndFromUP]),

    ok.

%%%--------------------------------------------------------------------
%%% @doc 
%%% Cancel during activate will cause node restart. <br/><br/>
%%% @spec remove_up(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
remove_up(_Config) ->
    MeId = swm_test_lib:get_me_id(?NC_Session),
    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    ct:pal("Remove upgrade package: : ~p",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_Session, "SUCCESS", MeId, Label),
    ok.


%%%--------------------------------------------------------------------
%%% Internal
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


cancel(Label, MeId) ->
    ct:pal("Perform Cancel.",[]),
    ct_netconfc:open(?NC_Session, []),
    case ct_netconfc:action(?NC_Session,
			    {'ManagedElement',
			     [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
			     [{managedElementId, [], [MeId]},
			      {'SystemFunctions',
			       [{systemFunctionsId,[],["1"]},
				{'SwM',
				 [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
				 [{swMId,[],["1"]},
				  {'UpgradePackage', [],
				   [{upgradePackageId, [Label]},
				    {cancel, [], []}]
				  }]}]}]}) of
	{ok, Res} ->
	    ct:pal(" ### Cancel result:~p, ~p",[Res, Label]);
	{error, closed} ->
	    ct:pal(" ### Cancel results in session is closed wery immedediatly"
		   " Nc session was closed, Don'care,~n"
		   "continue the test.",[]);
	_Other ->
	    ct:pal(" ### Cancel will fail result:~p, ~p",[_Other]),
	    ct:fail(" ### Cancel failed :~p.")
    end,
    ct_netconfc:close_session(?NC_Session).

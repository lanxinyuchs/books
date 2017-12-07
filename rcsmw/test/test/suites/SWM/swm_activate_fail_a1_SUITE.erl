%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_activate_fail_a1_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R2A/R3A/R4A/1
%%% 
%%% @doc == Test Suite for Upgrade Mechanism, activate fails due to prepare phase not done.  ==
%%% <br/><br/>
%%% @end

-module(swm_activate_fail_a1_SUITE).
-vsn('/main/R2A/R3A/R4A/1').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
%%% R2A/2      2014-02-03 etxivri     Created
%%% R2A/2      2014-04-15 etxivri     Update due to TR HS28038 is solved.
%%% R2A/4      2014-04-16 etxivri     Update to trig fallback.
%%% R2A/5      2014-04-23 etxivri     More update to handle fallback.
%%% R2A/6      2014-05-06 etxivri     Use reboot to trig fallback.
%%% R2A/7      2014-05-26 etxivri     Use reboot -f to trig fallback.
%%% R2A/8      2014-06-04 etxivri     Use cancel to trig fallback.
%%% R2A/9      2014-06-04 etxivri     Minor cleanup.
%%% R2A/10     2014-07-09 etxivri     Update ERROR log filter.
%%% R2A/11     2014-08-11 etxivri     Update ERROR log filter.
%%% R2A/12     2014-08-15 etxivri     Update due to new behaviour.
%%% R3A/1      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/2      2015-05-28 etxkols     Changed rct_netconf hook format
%%% R4A/1      2015-10-01 etxkols     A try to make it more robust.
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
-export([activate_fail_due_prepare_not_done/1]).

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
					  [%% %%"swmServer: throw {wrongState,1}",
					  %% "swmServer: throw eexist",
					  %% "Activate can not be executed in state INITIALIZED"
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
    [activate_fail_due_prepare_not_done].

%%%--------------------------------------------------------------------
%%% @doc 
%%% Activate will fail if prepare is not done.
%%% - Check also that it is ok to run create,prepare,activate again.
%%% <br/><br/>
%%% @spec activate_fail_due_prepare_not_done(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
activate_fail_due_prepare_not_done(_Config) ->
    ct:pal("UG, activate fails due to prepare phase not done",[]),

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

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    %%%%
    %% Activates an upgrade package.
    %% During activate the node will reboot.
    %%%%
    ct:pal("Activate when state is INITIALIZED shal result in failure.",[]),
    ExpErrInfoStr = "Activate can not be executed in state INITIALIZED",
    Res = swm_test_lib:up_action_generic_no_check(?NC_Session, 
						  activate, 
						  Label, 
						  MeId),

    swm_test_lib:check_exp_reply_err_from_nc_action(Res, 
						    ExpErrInfoStr),

    %% %% Check activate SUCCESS after create, prepare is done.
    %%%%
    %% Create UG
    %%%%
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),
    swm_test_lib:ug_create_match_result(?NC_Session, 
					"FAILURE",   %% Already created.
					%% "SUCCESS",
					?SftpHost, 
					?SftpUser, 
					?SftpPassword, 
					UGPath, 
					MeId), 

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
    %% Activate again
    %%%%
    swm_test_lib:ug_action_match_result(?NC_Session, 
    					"SUCCESS",
    					Label,
    					MeId, 
    					activate),

    %% %% %% Clean up after activate SUCCESS
    %% %%%%
    %% %% Reboot to trig fallback
    %% %%%%
    %% timer:sleep(3000),
    %% ct:pal("### reboot to trig fallback!",[]),
    %% ok = rct_rs232:login(console),
    %% net_kernel:disconnect(ErlNode),
    %% ok = ct_telnet:send(console, "reboot -f"),

    %%%%
    %% Cancel to trig fallback
    %%%%
    timer:sleep(10000),
    ct:pal("### trig fallback using cancel",[]),
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					cancel, 
    					Label, 
    					MeId),

    %% test_server:break("A"),

    %% %%%%
    %% %% pkill to trig fallback
    %% %%%%
    %% timer:sleep(3000),
    %% ct:pal("### trig fallback!",[]),
    %% swm_test_lib:pkill_to_trig_fallback(rpc_1),
    swm_test_lib:wait_for_node_state(ErlNode, down, 180000), %% wait max 3min
    {ok,_} = ct_telnet:expect(console, "Restarting system", 
    			      [{timeout,60000}, no_prompt_check]),
    {ok,_} = ct_telnet:expect(console, "login:", 
    			      [{timeout,60000}, no_prompt_check]),
    %%%%
    %% check node restarts
    %%%%
    swm_test_lib:wait_for_node_state(ErlNode, up),
    net_kernel:connect(ErlNode),
    swm_test_lib:check_nc_session(?NC_Session),

    ActionId_4 = no_check,
    [{"FAILURE", 
      "activate", 
      "The action was interrupted by a restart", 
      _D3, _E3, _ProgReport3}] =
    	swm_test_lib:wait_for_progress_result(?NC_Session, 
					      MeId, 
					      ActionId_4, 
					      ErlNode),

    %%%%
    %% Check fallback is done
    %%%%
    EndFromUP = swm_test_lib:get_sw_version(?NC_Session, MeId),
    case FromUP of 
    	EndFromUP ->
    	    ct:pal("SwVersionMain is same as it was in beginning.~n"
    		   "Fallback has been performed!"),
    	    ok;
    	_Res ->
    	    ct:pal("NOK: ~p", [_Res]),
    	    ct:fail("NOK")
    end,

    %%%%
    %% Remove the created up.
    %%%%
    ct:pal("Remove upgrade package: : ~s",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_Session, "SUCCESS", MeId, Label),

    ok.

%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_rob_activate_reboot_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R7A/1
%%% 
%%% @doc == After expected restart due to activate, then perform unexpected reboot.  ==
%%% <br/><br/>
%%% @end

-module(swm_rob_activate_reboot_SUITE).
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
%%% R2A/3      2014-04-14 etxivri     Update due to tr HS43738 is solved.
%%% R2A/4      2014-04-14 etxivri     Changed back error log filter due to
%%%                                   HS43738 was not involved in this ERROR.
%%% R2A/5      2014-04-14 etxivri     Update result after unexpected reboot.
%%% R2A/6      2014-05-07 etxivri     Update random reboot.
%%% R2A/8      2014-06-04 etxivri     Changed check for "du1 login" prompt to 
%%%                                   "login:" 
%%% R2A/9      2014-06-26 etxivri     Added wait for ntp sync
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
	 random_reboot/1]).

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
					  ["post_activate",
					   "** Generic server swmServer terminating",
					  "crasher:"
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
     random_reboot].

groups() ->
    [{group_1,[],[build_to_up,
		  random_reboot,
		  random_reboot,
		  random_reboot
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
random_reboot(_Config) ->
    %%%%
    %% Get a random nr between 1-5, that will be used in sleep before reboot.
    %%%%
    Random = rand:uniform(5)+10,
    ct:pal("TC will use Random : ~p",[Random]),
    activate_fail_due_to_random_reboot(Random).

%%%--------------------------------------------------------------------
%%% @doc 
%%% @end
%%%--------------------------------------------------------------------
activate_fail_due_to_random_reboot(Random) ->
    ct:pal("UG. unexpected reboot after restart in activate phase.",[]),

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

    %%%%
    %% Activates an upgrade package.
    %% During activate the node will reboot.
    %%%%
    ActionId = swm_test_lib:get_action_id(?NC_Session, MeId),
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					activate, 
    					Label, 
    					MeId),

    %%%%
    %% check node restarts
    %%%%
    net_kernel:disconnect(ErlNode),
    timer:sleep(5000),
    swm_test_lib:wait_for_node_state(ErlNode, down),
    RestartStr = swm_test_lib:get_node_restart_str(),
    ct_telnet:expect(console, RestartStr,
		     [{timeout, 60000}, no_prompt_check]),
    {ok,_} = ct_telnet:expect(console, "login:", 
			      [{timeout,60000}, no_prompt_check]),
    ct:pal("Login prompt rceived. reboot node soon!"),
    %% net_kernel:connect(ErlNode),

    ct:pal("sleep : ~p, then reboot node.",[Random]),
    ct:sleep({seconds, Random}),
    
    %%%%
    %% Reboot node.
    %%%%
    ct:pal("### reboot!",[]),
    rct_rs232:login(console),
    ok = ct_telnet:send(console, "reboot"),
    timer:sleep(5000),
    %%%%
    %% check node restarts
    %%%%
    swm_test_lib:wait_for_node_state(ErlNode, down),
    ct_telnet:expect(console, RestartStr,
    		     [{timeout, 60000}, no_prompt_check]),

    {ok,_} = ct_telnet:expect(console, "login:", 
			      [{timeout,60000}, no_prompt_check]),
    swm_test_lib:wait_for_node_state(ErlNode, up),
    net_kernel:connect(ErlNode),


    swm_test_lib:check_nc_session(?NC_Session),
    ct:pal("Wait for progress after reboot.", []),

    case swm_test_lib:
    	wait_for_progress_result(?NC_Session, MeId, ActionId, ErlNode) of
    	[{"FAILURE" = Res, "activate" = ActionName, 
    	  %% "The action was interrupted by a restart" = ProgressInfo, 
	  %% "Post activation procedure failed" = ProgressInfo,
	  ProgressInfo,
    	  "The action was interrupted by a restart" = ResultInfo, 
	  State, 
	  _ProgReport}] ->
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

    %% ct:pal("Fallback will be performed. Wait for node restart"),
    %% swm_test_lib:wait_for_node_state(ErlNode, down),
    ct:pal("Node shall startup on from up. "),
    swm_test_lib:wait_for_node_state(ErlNode, up),
    net_kernel:connect(ErlNode),

    swm_test_lib:check_nc_session(?NC_Session),
    ct_netconfc:close_session(?NC_Session),

    %%%%
    %% Check fallback has been performed.
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
    ct:pal("Remove upgrade package: : ~p",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_Session, "SUCCESS", MeId, Label),
    
    swm_test_lib:wait_for_ntp_synch(rpc_1),

    ok.

%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------

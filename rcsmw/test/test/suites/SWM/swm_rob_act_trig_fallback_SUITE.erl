%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_rob_act_trig_fallback_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R2A/R3A/2
%%% 
%%% @doc == Perform activate phase in upgrade. Trig fallback using reboot the node. Updates of cxs label in to up is handled in TC.  ==
%%% <br/><br/>
%%% @end

-module(swm_rob_act_trig_fallback_SUITE).
-vsn('/main/R2A/R3A/2').

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
%%% R2A/2      2014-04-10 etxivri     Created
%%% R2A/4      2014-05-19 etxivri     Remove use of failsafe.
%%% R2A/5      2014-06-04 etxivri     Minor cleanup.
%%% R3A/1      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/2      2015-05-28 etxkols     Changed rct_netconf hook format 
%%%                                   
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
-export([
	 build_to_up/1,
	 act_trig_fallback_1/1
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
		 {cth_conn_log, []},
		 %% {rct_rs232,console},
		 {rct_core,[]},
                 {rct_logging, {upgrade, 
				[{erlang,{[
					   "ERROR REPORT",
					   "CRASH REPORT"
					  ],
					  []
					 }}]}},
		 {rct_cli, {cli1, [manual_connect]}},
		 {rct_netconf,nc1}]}].


%% @hidden
init_per_suite(Config) ->
    %% ct:log("# init per suite. ~n"
    %% 	   "Create cxps that shall be used for UG."), 
    %% swm_test_lib:build_valid_ug_packakage(?NC_Session),

    DF = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    Df = string:tokens(DF, " \n"),
    ct:pal("### Df: ~p", [Df]),

    Config.

%% @hidden
end_per_suite(_Config) ->
    DF1 = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    Df1 = string:tokens(DF1, " \n"),
    ct:pal("### Df: ~p", [Df1]),

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
    	    	   "Clean up! TBD!", [Reason])
    	    %% %% Remove created upgrade package.
	    %% swm_test_lib:check_nc_session(?NC_Session),
	    %% [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    	    %% ct:pal("Label:~n~p~n",[Label]),
    	    %% swm_test_lib:remove_upgrade_package(?NC_Session, Label)
		
    end,
    
    ok.


%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() -> 
    [build_to_up, 
     act_trig_fallback_1
    ].

groups() ->
    [{group_1,[],[build_to_up,
		  act_trig_fallback_1
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
%%% @doc 
%%% - 
%%% <br/><br/>
%%% @end
%%%--------------------------------------------------------------------
act_trig_fallback_1(_Config) ->
    ct:pal("UG.",[]),
    
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    FromUP = swm_test_lib:get_sw_version(?NC_Session, MeId),
    ct:pal("FromUP: ~p",[FromUP]),

    %%%%
    %% Modify version in cxs to UP.
    %%%%
    modify_cxs(MeId),

    %% %%%%
    %% %% create, prepare, verify, activate.
    %% %%%%

    create(MeId),

    %%%%
    %% Get Latest UP
    %%%%
    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    ct:pal("Label:~n~p~n",[Label]),

    prepare(MeId, Label),
    verify(MeId, Label),
    activate(MeId, Label),

    %% test_server:break("Activate done. Continue to trig fallback, "
    %% 		      "using reboot node.B"),

    %% %%%%
    %% %% Reboot node.
    %% %%%%
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    ct:pal("ErlNode : ~p", [ErlNode]),
    %% ct:pal("### reboot!",[]),
    %% ok = rct_rs232:login(console),
    %% net_kernel:disconnect(ErlNode),
    %% ok = ct_telnet:send(console, "reboot"),
    %% %%%%
    %% %% check node restarts
    %% %%%%
    %% swm_test_lib:wait_for_node_state(ErlNode, down),
    %% swm_test_lib:wait_for_node_state(ErlNode, up),
    %% net_kernel:connect(ErlNode),

    %%%%
    %% pkill to trig fallback
    %%%%
    timer:sleep(3000),
    ct:pal("### trig fallback!",[]),
    swm_test_lib:pkill_to_trig_fallback(rpc_1),
    swm_test_lib:wait_for_node_state(ErlNode, down, 180000), %% wait max 3min

    %%%%
    %% check node restarts
    %%%%
    swm_test_lib:wait_for_node_state(ErlNode, up),
    net_kernel:connect(ErlNode),
    swm_test_lib:check_nc_session(?NC_Session),

    %% test_server:break("Check Fallback has been performed.!!!!"),

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
    %% Remove Last label
    %%%%
    remove(MeId),

    ok.

%%%--------------------------------------------------------------------
%%% @doc Modify cxs.
%%% @end
%%%--------------------------------------------------------------------
modify_cxs(MeId) ->
    ct:log("# init per suite. ~n"
    	   "Create cxps that shall be used for UG."), 

    SW_Vers = swm_test_lib:get_sw_version(?NC_Session, MeId),
    ct:pal("A_CXS:~n~p~n",[SW_Vers]),

    %% %% Construct a CXS label.
    [CXS, Index, ALabel] = string:tokens(SW_Vers, "/-"),
    ct:pal("CXS:~p, Index: ~p, ALabel: ~p ~n",[CXS, Index, ALabel]),

    CxsUp = "cxs101549_"++Index++"-up.xml",
    ct:pal("CxsUp: ~p ", [CxsUp]),

    %% %% Get STP name
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),

    %% %% Increase label version with 1.
    {R,Nr}= lists:split(3,ALabel), %% Split R2A
    ToVer = R ++ integer_to_list(list_to_integer(Nr)+1),
    ct:pal("ToVer: ~p ", [ToVer]),

    CMD1 = "sed 's/"++ALabel++"/"++ToVer++"/' "++UGPath++"/"++
    	CxsUp++" > "++ UGPath++"/tmp_cxs101549-up.xml",
    ct:pal("CMD1:~n~p~n",[CMD1]),
    GG = os:cmd(CMD1),
    ct:pal("GG:~n~p~n",[GG]),

    CMD2 = "mv "++UGPath++"/tmp_cxs101549-up.xml "++UGPath++"/"++CxsUp,
    ct:pal("CMD2:~n~p~n",[CMD2]),
    os:cmd(CMD2).


%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
create(MeId) ->
    ct:pal("Create.",[]),

    %% SW_Vers = swm_test_lib:
    %% 	get_specific_reportprogress_info(?CLI_Session, "SwVersionMain"),
    SW_Vers = swm_test_lib:get_sw_version(?NC_Session, MeId),
    ct:pal("A_CXS:~n~p~n",[SW_Vers]),

    DF = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    Df = string:tokens(DF, " \n"),
    ct:pal("### Df: ~p", [Df]),

    %% %%%%
    %% %% Create UG
    %% %%%%
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),
    ct:pal("create",[]),
    %% swm_test_lib:ug_create_match_result(?NC_Session, 
    %% 					"SUCCESS", 
    %% 					?SftpHost, 
    %% 					?SftpUser, 
    %% 					?SftpPassword, 
    %% 					UGPath, 
    %% 					MeId), 


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
	    ct:log("result:~p~n"
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
    


    %% DF1 = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    %% Df1 = string:tokens(DF1, " \n"),
    %% ct:pal("### Df: ~p", [Df1]),

    ok.

%%%--------------------------------------------------------------------
%%% @doc Prepare.
%%% @end
%%%--------------------------------------------------------------------
prepare(MeId, Label) ->
    ct:pal("Prepare.",[]),

    %%%%
    %% Get Latest UP
    %%%%
    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    ct:pal("Label:~n~p~n",[Label]),

    %%%%
    %% Prepares an upgrade package, 
    %% which means downloading a complete UP
    %%%%
    ct:pal("prepare: : ~s",[Label]),
    swm_test_lib:ug_action_match_result(?NC_Session, 
    					"SUCCESS",
    					Label,
    					MeId, 
    					prepare),

    %% DF1 = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    %% Df1 = string:tokens(DF1, " \n"),
    %% ct:pal("### Df: ~p", [Df1]),

    ok.

%%%--------------------------------------------------------------------
%%% @doc Verify.
%%% @end
%%%--------------------------------------------------------------------
verify(MeId, Label) ->
    ct:pal("Verify.",[]),

    %%%%
    %% Get Latest UP
    %%%%
    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    ct:pal("Label:~n~p~n",[Label]),

    %%%%
    %% Prepares an upgrade package, 
    %% which means downloading a complete UP
    %%%%
    ct:pal("verify: : ~s",[Label]),
    swm_test_lib:ug_action_match_result(?NC_Session, 
    					"SUCCESS",
    					Label,
    					MeId, 
    					verify),

    %% DF1 = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    %% Df1 = string:tokens(DF1, " \n"),
    %% ct:pal("### Df: ~p", [Df1]),

    ok.

%%%--------------------------------------------------------------------
%%% @doc Activate.
%%% @end
%%%--------------------------------------------------------------------
activate(MeId, Label) ->
    ct:pal("activate.",[]),

    %%%%
    %% Prepares an upgrade package, 
    %% which means downloading a complete UP
    %%%%
    ct:pal("activate: : ~s",[Label]),
    swm_test_lib:ug_action_match_result(?NC_Session, 
    					"SUCCESS",
    					Label,
    					MeId, 
    					activate),

    ok.


%%%--------------------------------------------------------------------
%%% @doc Remove Latest UPs.
%%% @end
%%%--------------------------------------------------------------------
remove(MeId) ->
    ct:pal("remove.",[]),

    %%%%
    %% Get Latest UPs
    %%%%
    %% [Label | _] = 
    %% 	lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    [_FirstLabel | LatestUPs] = swm_test_lib:get_ups(?NC_Session),
    ct:pal("Label:~n~p~n",[LatestUPs]),
    OldActionId = no_check,
    lists:foreach(fun(Label)->
			  ct:pal("Remove upgrade package: : ~s",[Label]),
			  %% swm_test_lib:remove_upgrade_package(?NC_Session, 
			  %% 				      "SUCCESS", 
			  %% 				      MeId, 
			  %% 				      Label)
			  swm_test_lib:remove_upgrade_package(?NC_Session, 
							      Label),
			  swm_test_lib:
			      wait_for_progress_done(?NC_Session, 
						     "SUCCESS", 
						     "removeUpgradePackage",
						     MeId, 
						     OldActionId)
		  end, LatestUPs),
    
    ok.


%%% Internal
%%%--------------------------------------------------------------------
%%% 
%%%--------------------------------------------------------------------

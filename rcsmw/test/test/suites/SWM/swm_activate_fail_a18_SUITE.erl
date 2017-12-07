%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_activate_fail_a18_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R4A/R6A/1
%%% 
%%% @doc == Test Suite for Upgrade Mechanism. Make sure activate does not fail when application trying to write wrong attribute type. ==
%%% <br/><br/>
%%% @end

-module(swm_activate_fail_a18_SUITE).
-vsn('/main/R2A/R3A/R4A/R6A/1').

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
%%% R2A/2      2014-02-28 etxivri     Created
%%% R2A/2      2014-04-16 etxivri     Update to trig fallback.
%%% R2A/6      2014-04-29 etxivri     Update to work when TR HS44810 is OK.
%%% R3A/1      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/2      2015-05-28 etxkols     Changed rct_netconf hook format
%%% R4A/1      2015-09-02 etxivri     Update due to changed behaviour.
%%% R4A/2      2015-09-08 etxivri     Cleanup in TC to avoid error on ct shell. 
%%% R6A/1      2016-08-18 etxkols     Git migration requires that CC paths is not used 
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
-export([activate_when_write_wrong_attr_type/1]).

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).
-define(NC_Session, nc1).
-define(ModScript_Path, rct_cc_git_path:find("RCT_TOP", ["test/suites/SWM/bin/"])).
-define(ModScriptName_A18, "swm_mod_fake_a18.sh").

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
				[{erlang,{["ERROR REPORT",
					   "CRASH REPORT"],
					  ["operation ABORTED because of unknown classes",
					   "wrong type on betaM",
					   "failed to insert in CCB",
					   "failed to initialize admin owner"
					  ]
					 }}]}},
		 {rct_netconf,nc1}]}].


%% @hidden
init_per_suite(Config) ->
    ct:pal("# init per suite. ~n"
    	   "Create cxps that shall be used for UG."), 
    swm_test_lib:build_faulty_ug_packakage(?NC_Session, 
					   ?ModScript_Path, 
					   ?ModScriptName_A18),
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
    		   "Clean up!", [Reason]),
	    %% Remove created upgrade package.
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
    [activate_when_write_wrong_attr_type].

%%%--------------------------------------------------------------------
%%% @doc 
%%% Activate an upgrade package. During activate a application trying <br/>
%%% to write wrong attribute type (int instead of string). <br/>
%%% This shall not make upgrade fail. <br/>
%%% The application will continue write correct attr. <br/>
%%% And the activate will be SUCCESS. <br/>
%%% @spec activate_when_write_wrong_attr_type(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
activate_when_write_wrong_attr_type(_Config) ->
    ct:pal("UG activate shall not fail when application trying to write wrong attribute type."
	   "supported in the XML structure.",[]),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    ct:pal("ErlNode : ~p", [ErlNode]),
    net_kernel:disconnect(ErlNode),

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

    %%%%
    %% Activates an upgrade package.
    %% - s2 application will try to write wrong attribute type.
    %% - application will recive ok, but the fault will be found later in check.
    %% - This results that activate phase shall result in fallback after 2 min.
    %% During activate the node will reboot.
    %%%%
    swm_test_lib:ug_action_match_result(?NC_Session, 
    					"FAILURE",
    					Label,
    					MeId, 
    					activate),

    %%%%
    %% Check fallback has been performed
    %%%%
    ActionId_4 = no_check,
    [{"FAILURE", 
      "activate", 
      "The action was interrupted by a restart", 
      _D3, _E3, _ProgReport3}] =
    	swm_test_lib:wait_for_progress_result(?NC_Session, 
					      MeId, 
					      ActionId_4, 
					      ErlNode),

    %% %%% Check expected fault cause is found in dummy log.
    %% %%% to make sure applications has been runed the fault.
    Cat = rct_rpc:call(
    	    rpc_1, os, cmd, 
    	    ["cat /rcs/applicationlogs/DUMMY-*_CXP9021691_*/log_ugt_s2*"], 
    	    10000, noprint),
    ct:pal("Cat:~n~p~n",[Cat]),
    BBBB = string:tokens(Cat,"\n, "),
    ct:pal("BBBB:~n~p~n",[BBBB]),
    case lists:member("fault_a18", BBBB) of
    	true ->
    	    ok;
    	false ->
    	    ct:fail("fault_a18 not found in DUMMY log")
    end,

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

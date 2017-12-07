%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_activate_fail_a17_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R4A/R6A/1
%%%
%%% @doc == Test Suite for Upgrade Mechanism. Make sure activate does not fail when application write to an attribute not supported in the XML structure. ==
%%% <br/><br/>
%%% @end

-module(swm_activate_fail_a17_SUITE).
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
%%% R2A/2      2014-02-24 etxivri     Created
%%% R2A/2      2014-03-25 etxivri     Update trigger of fallback.
%%% R2A/4      2014-04-16 etxivri     New way to trig fallback.
%%% R2A/5      2014-04-22 etxivri     Updtade to handle expected fallback.
%%% R2A/6      2014-04-24 etxivri     Update match on string and handle fallback
%%% R3A/1      2014-11-13 erarafo     Description of fallback behavior updated
%%% R3A/2      2015-01-29 etxivri     Update to remove error in ct-shell.
%%% R3A/3      2015-03-12 etxivri     Update error log filter.
%%% R3A/4      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/5      2015-05-28 etxkols     Changed rct_netconf hook format
%%% R4A/1      2015-09-03 etxivri     Update due to changed behaviour.
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
-export([activate_when_write_attr_not_supported/1]).

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).
-define(NC_Session, nc1).
-define(ModScript_Path, rct_cc_git_path:find("RCT_TOP", ["test/suites/SWM/bin/"])).
-define(ModScriptName_A17, "swm_mod_fake_a17.sh").

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
					  ["failed to insert in CCB"
					  ]
					 }}]}},
		 {rct_netconf,nc1}]}].


%% @hidden
init_per_suite(Config) ->
    ct:pal("# init per suite. ~n"
    	   "Create cxps that shall be used for UG."),
    swm_test_lib:build_faulty_ug_packakage(?NC_Session,
					   ?ModScript_Path,
					   ?ModScriptName_A17),
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
%%% @doc Runs all testcases in SUITE. On target.
%%% @end
%%%--------------------------------------------------------------------
all() ->
    [activate_when_write_attr_not_supported].

%%%--------------------------------------------------------------------
%%% @doc
%%% Activate an upgrade package. During activate a application write an attribute not<br/>
%%% supported in the XML structure. This shall not make upgrade fail. <br/>
%%% The application will continue write correct attr that exist. <br/>
%%% And the activate will be SUCCESS. <br/>
%%% @spec activate_when_write_attr_not_supported(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
activate_when_write_attr_not_supported(_Config) ->
    ct:pal("UG activate shall not fail when application write to an attribute not "
	   "supported in the XML structure.",[]),
    Up = rct_rpc:call(rpc_1, swmI, get_current_up_metadata, [], 10000, noprint),
    ct:pal("Current UP:~n~p~n",[Up]),

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

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    ct:pal("ErlNode : ~p", [ErlNode]),

    %%%%
    %% Activates an upgrade package.
    %% - s1v2 application will try to write a non existing classs.
    %% - Then it will continue.
    %% - the activate phase shall result in fallback promptly.
    %% There will be two reboots in total: the first one is part
    %% of the upgrade attempt, and the second is the fallback.
    %%%%
    net_kernel:disconnect(ErlNode),
    swm_test_lib:ug_action_match_result(?NC_Session,
    					"FAILURE",
    					Label,
    					MeId,
    					activate),

    %% ok = swm_test_lib:up_action_generic(?NC_Session,
    %% 					activate,
    %% 					Label,
    %% 					MeId),

    %% {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    %% ct:pal("ErlNode : ~p", [ErlNode]),

    ActionId_4 = no_check,
    [{"FAILURE",
      "activate",
      "The action was interrupted by a restart",
      _D3, _E3, _ProgReport3}] =
    	swm_test_lib:wait_for_progress_result(?NC_Session,
					      MeId,
					      ActionId_4,
					      ErlNode),

    %% %%% Check expected ok result is found in dummy log.
    %% %%% to make sure applications has been runed the fault.
    Cat = rct_rpc:call(
    	    rpc_1, os, cmd,
    	    ["cat /rcs/applicationlogs/DUMMY-*_CXP9021691_*/log_ugt_s1v2*"],
    	    10000, noprint),
    ct:log("Cat:~n~p~n",[string:tokens(Cat,"\n, ")]),
    BBBB = string:tokens(Cat,"\n, "),

    CCCC =  lists:flatten(BBBB),
    ct:log("Flatten Cat:~n~p~n",[CCCC]),
    %% case re:run(CCCC,"okwhenwritenotsupportedattribute") of
    case re:run(CCCC,"codeSA_AIS_OKeventhoughaninvalidattributename") of
    	{match,_} ->
    	    ok;
    	nomatch ->
    	    ct:fail("ok when write not supported attribute, "
		    "not found in DUMMY log")
    end,

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

%c% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_ug_mod1_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R6A/1
%%%
%%% @doc == Test Suite for testing various software management related stuff ==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% rct_netconf is used in ct_hooks:
%%% see <a href="file://$RTC_TOP/test/lib/supp/esrc/rct_netconf.erl">rct_netconf.erl</a><br/>
%%% @end

-module(swm_ug_mod1_SUITE).
-vsn('/main/R2A/R3A/R6A/1').

%%% ----------------------------------------------------------
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
%%% R2A/2      2014-01-30 etxivri     Same as swm_SUITE, exept added
%%%                                   create modified toUP in init_per_suite,
%%%                                   instead that jenkins build mod toUP.
%%% R2A/3      2014-04-08 erarafo     Deprecation warning
%%% R2A/4      2014-05-23 etxivri     Changed commit to confirm.
%%% R2A/5      2014-07-23 etxjotj     New confirm behaviour, 
%%%                                   new progress reporting
%%% R2A/6      2014-09-25 etxivri     Update when get the UP label to be used.
%%% R3A/1      2014-10-09 etxkols     Added console logging
%%% R3A/2      2014-12-01 etxivri     Update records due to changed model.
%%% R3A/3      2014-12-04 etxivri     Update to be more generic.
%%% R3A/4      2015-01-13 eransbn     Removed rpc call.
%%% R3A/5      2015-01-29 etxivri     Update to prevent error in ct shell.
%%%                                   when node restarts during activate.
%%% R3A/6      2015-05-21 etxkols     Fix for 2 labs
%%% R3A/7      2015-05-29 etxkols     Changed rct_netconf hook format 
%%% ----------------------------------------------------------

%-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).
%% -export([create/1, prepare/1, verify/1, activate/1, confirm/1, clean_up/1]).
-export([create/1, prepare/1, verify/1, activate/1, confirm/1]).

%% -define(NC_Session, nc1).
-define(ModScript_Path, rct_cc_git_path:find("RCT_TOP", ["test/suites/SWM/bin/"])).

-define(ModScriptName_1, "swm_mod_fake_cxp.sh").


%% -define(SftpHost, "10.68.200.11").
%% -define(SftpUser, "mauve").
%% -define(SftpPassword, "dilbert").
-define(NC_Session, nc1).
-define(CLI_Session, cli1).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf.
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{ct_hooks, [{rct_htmllink,[]},
	%%	 {rct_rpc, rpc_1},
		 {rct_upgrade,ug1},
		 {rct_core,[]},
                 {rct_logging, {upgrade, [{erlang,{["ERROR REPORT",
						    "CRASH REPORT"],[]}}]}},
		 {rct_cli, {cli1, [manual_connect]}},
		 {rct_netconf,nc1},
                 {rct_rs232,console},
                 {cth_conn_log,[]}]}].


%% @hidden
init_per_suite(Config) ->
    ct:pal("# init per suite. ~n"
    	   "Create cxps that shall be used for UG."),
    swm_test_lib:build_faulty_ug_packakage(?NC_Session,
    					   ?ModScript_Path,
    					   ?ModScriptName_1),
    Config.

%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    %% Up = rct_rpc:call(rpc_1, swmI, get_current_up_metadata, [], 10000, noprint),
    %% ct:pal("Current UP:~n~p~n",[Up]),
    Up = get_current_up_data(),
    ct:pal("Current UP:~n~p~n",[Up]),
    Config.
%% @hidden
end_per_testcase(_TestCase, Config) ->
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p.  \n", [Reason])
    end,
    ok.


%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() ->
    [create, prepare, verify, activate, confirm].

%%%--------------------------------------------------------------------
%%% @doc Create an upgrade package, using the latest package found in clearcase
%%% @end
%%%--------------------------------------------------------------------

create(_Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Create UG
    %%%%
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),

    [{host, SftpHost},{username, Username},{password, Password}] = ct:get_config(sftp_server),
    swm_test_lib:ug_create_match_result(?NC_Session,
    					"SUCCESS",
    					SftpHost,
    					Username,
    					Password,
    					UGPath,
    					MeId),
    ok.

%%%--------------------------------------------------------------------
%%% @doc Prepares an upgrade package, which means downloading a complete UP
%%% @end
%%%--------------------------------------------------------------------

prepare(_Config) ->
    %%%%
    %% Prepares an upgrade package,
    %% which means downloading a complete UP
    %%%%
    perform_ug_action(prepare),
    ok.

%%%--------------------------------------------------------------------
%%% @doc Verifies an upgrade package
%%% @end
%%%--------------------------------------------------------------------

verify(_Config) ->
    %%%%
    %% Verifies an upgrade package.
    %%%%
    perform_ug_action(verify),
    ok.

%%%--------------------------------------------------------------------
%%% @doc Activates an upgrade package
%%% @end
%%%--------------------------------------------------------------------

activate(_Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Get Latest UP
    %%%%
    Label = get_latest_up(),

    %%%%
    %% Activates an upgrade package.
    %% During activate the node will reboot.
    %%%%
    %% perform_ug_action(activate),
    ct:pal("Perform ug action: activate", []),
    swm_test_lib:ug_action_match_result(?NC_Session,
    					"SUCCESS",
    					Label,
    					MeId,
    					activate,
					dummy,
					console),
    ok.

%%%--------------------------------------------------------------------
%%% @doc Confirms an upgrade package
%%% @end
%%%--------------------------------------------------------------------

confirm(_Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Get Latest UP
    %%%%
    Label = get_latest_up(),

    ct:pal("Perform ug confirm~n",[]),
    swm_test_lib:ug_confirm(?NC_Session,
			    Label,
			    MeId),
    ok.

%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------
get_latest_up() ->
    %% [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    %% ct:pal("Label:~n~p~n",[Label]),

    UPs = swm_test_lib:get_ups(?NC_Session),
    ct:log("UPs:~n~p~n",[UPs]),

    Label = swm_test_lib:get_highest_label(UPs),
    ct:log("Label:~n~p~n",[Label]),
    Label.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
perform_ug_action(Action) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Get Latest UP
    %%%%
    Label = get_latest_up(),

    %%%%
    %% Perform ug action
    %%%%
    ct:pal("Perform ug action:~n~p~n",[Action]),
    swm_test_lib:ug_action_match_result(?NC_Session,
    					"SUCCESS",
    					Label,
    					MeId,
    					Action),
    ok.
get_current_up_data()->
    ok = rct_cli:connect(?CLI_Session),
    rct_cli:send(?CLI_Session,"configure"),
    {ok ,RecievedData} = 
	rct_cli:send(?CLI_Session,
		     "show ManagedElement=1,SystemFunctions=1,SwM=1"),
    ok = rct_cli:disconnect(?CLI_Session),
    string:tokens(RecievedData, "=\r\n ").

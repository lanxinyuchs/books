%%coding: latin-1
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_commit_fail_c1_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R6A/1
%%% 
%%% @doc == Test Suite for Upgrade Mechanism, fallback is performed due to no confirm action performed.  ==
%%% <br/><br/>
%%% @end

-module(swm_commit_fail_c1_SUITE).
-vsn('/main/R2A/R3A/R6A/1').

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
%%% R2A/2      2014-01-23 etxivri     Created
%%% R2A/3      2014-01-27 etxivri     Added net_kernel disconnect/connect,
%%%                                   To handle ERROR when node goes down.
%%%                                   Also updates due to cahnges in lib.
%%%                                   
%%% R2A/5      2014-03-19 etxjotj     Added alternate method of reading alarms
%%% R2A/6      2014-03-19 etxivri     Use of new method reading alarms from
%%%                                   swm_test_lib
%%% R2A/7      2014-03-20 etxivri     Increased timer before check alarm exist.
%%% R2A/9      2014-05-23 etxivri     Changed commit to confirm.
%%% R2A/10     2014-06-12 etxivri     Changed commit when cli config.
%%% R2A/11     2014-06-27 etxivri     Added coding: latin-1
%%% R3A/1      2015-01-29 etxivri     Update to remove error in ct-shell.
%%% R3A/2      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/3      2015-05-28 etxkols     Changed rct_netconf hook format
%%% R6A/1      2016-09-14 etxivri     Update due to chanaged behaviour.
%%%                                   delafult fallback time is now 1800 sec and
%%%                                   if set lower than that it will set back to
%%%                                   default value after activate.
%%% ----------------------------------------------------------

%-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).
-export([confirm_fallback_alarm_not_removed/1]).

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).
-define(FbOpStSo_MiTy, "9175043").  %% FallbackOperationStartingSoon, 
                                    %% minorType=9175043
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
    Config.

%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_testcase(confirm_fallback_alarm_not_removed, Config) ->
    ct:pal("# init per suite. ~n"
    	   "Create cxps that shall be used for UG."), 
    swm_test_lib:build_valid_ug_packakage(?NC_Session),
    Config;
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(confirm_fallback_alarm_not_removed, Config) ->
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
      	    ct:pal("Testcase failed due to: ~p.  \n"
    		   "Clean up!", [Reason]),
    	    %% Remove created upgrade package.
	    swm_test_lib:check_nc_session(?NC_Session),
    	    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    	    ct:pal("Label:~n~p~n",[Label]),
    	    swm_test_lib:remove_upgrade_package(?NC_Session, Label)

    end,
    ok;
end_per_testcase(_TestCase, _Config) ->
    ok.


%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() -> 
    [confirm_fallback_alarm_not_removed].

%%%--------------------------------------------------------------------
%%% @doc 
%%% Fallback is performed due to no confirm action performed.
%%% - SwmFallbackOperationStartingSoonAlarm will be generated when 
%%%   timeRemainingBeforeFallback is 1800 sec.
%%% - When timeRemainingBeforeFallback is 0 then fallback is performed.
%%% <br/><br/>
%%% @spec confirm_fallback_alarm_not_removed(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
confirm_fallback_alarm_not_removed(_Config) ->
    ct:pal("UG Confirm fails,"
	   "SwmFallbackOperationStartingSoonAlarm not removed.",[]),
    Up = rct_rpc:call(rpc_1, swmI, get_current_up_metadata, [], 10000, noprint),
    ct:pal("Current UP:~n~p~n",[Up]),

    %%%%
    %% Decrease fallbackTimer to 420 sec
    %%%%
%%    set_fallbacktimer(?CLI_Session, "420"),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    ct:pal("ErlNode : ~p", [ErlNode]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Check minorType "9175043" (FallbackOperationStartingSoon) not exist.
    %%%%    
    case swm_test_lib:is_alarm(?NC_Session, MeId, ?FbOpStSo_MiTy) of
	false ->
	    ok;
	true ->
	    ct:fail("Tc will fail, FallbackOperationStartingSoon shall "
		    "not exists")
    end,

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
    %% During activate the node will reboot.
    %%%%
    net_kernel:disconnect(ErlNode),
    swm_test_lib:ug_action_match_result(?NC_Session, 
					"SUCCESS",
					Label,
					MeId, 
					activate),
    %%%%
    %% Don't send confirm.
    %% Check alarm "A Fallback Operation will soon be started" is generated.
    %% Shall be generated timeRemainingBeforeFallback 300 sec. 
    %%%%
    ct:pal("Check Alarm, ¤ A Fallback Operation will soon be started ¤, "
    	   "is generated.~n Shall be generated after 1500 sec. 300 sec left", []),
    ct:sleep({seconds, 10}),
    wait_for_time_remaining_before_fallback(?CLI_Session, 300),
    ct:sleep({seconds, 30}),

    %%%%
    %% Check minorType "9175043" (FallbackOperationStartingSoon) exist.
    %%%%    
    case swm_test_lib:is_alarm(?NC_Session, MeId, ?FbOpStSo_MiTy) of
	true ->
	    ok;
	false ->
	    ct:fail("Tc will fail, FallbackOperationStartingSoon not exists")
    end,

    %%%%
    %% Wait 300 sec and check fallback is performed.
    %%%%
    ct:pal("Check fallback is performed after 300sec"),
    ct:sleep({seconds, 10}),
    wait_for_time_remaining_before_fallback(?CLI_Session, 10),

    %%%%
    %% check node restarts
    %%%%
    net_kernel:disconnect(ErlNode),
    swm_test_lib:wait_for_node_state(ErlNode, down),
    swm_test_lib:wait_for_node_state(ErlNode, up),
    net_kernel:connect(ErlNode),

    swm_test_lib:check_nc_session(?NC_Session),
    EndFromUP = swm_test_lib:get_sw_version(?NC_Session, MeId),

    case FromUP of 
    	EndFromUP ->
    	    ct:pal("SwVersion is same as it was in beginning.~n"
    		   "Fallback has been performed!"),
    	    ok;
    	_Res ->
    	    ct:pal("NOK: ~p", [_Res]),
    	    ct:fail("NOK")
    end,

    %%%%
    %% Check Alarm is ceased.
    %%%%
    ct:pal("Check Alarm, ¤ A Fallback Operation will soon be started ¤, "
	   "shall be ceased"),
    %%%%
    %% Check minorType "9175043" (FallbackOperationStartingSoon) not exist.
    %%%%    
    case swm_test_lib:is_alarm(?NC_Session, MeId, ?FbOpStSo_MiTy) of
	false ->
	    ok;
	true ->
	    ct:fail("Tc will fail, FallbackOperationStartingSoon shall "
		    "not exists")
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

%%%--------------------------------------------------------------------
%%% Description: wait for progress and match expected result.
%%%--------------------------------------------------------------------
%%set_fallbacktimer(CLI_Session, Seconds) ->
%%    ct:pal("### Set fallbackTimer to: ~p .",[Seconds]),
%%    cli_connect(CLI_Session),
%%    rct_cli:send(CLI_Session,"configure"),
%%    rct_cli:send(CLI_Session,"ManagedElement=1,SystemFunctions=1,SwM=1"),
%%    rct_cli:send(CLI_Session,"fallbackTimer="++Seconds),
%%    rct_cli:send(CLI_Session,"commit"),
%%    rct_cli:send(CLI_Session,"top"),

%%    {ok ,RecievedData} = 
%%	rct_cli:send(CLI_Session,
%%		     "show ManagedElement=1,SystemFunctions=1,SwM=1"),
%%    Data = string:tokens(RecievedData, "=\r\n "),
%%    ct:log("show : ~p", [Data]),
%%
%%    cli_disconnect(CLI_Session),
%%    ok.

%%%--------------------------------------------------------------------
%%% Description: 
%%%--------------------------------------------------------------------
get_time_remaining_before_fallback(CLI_Session) ->
    Seconds = swm_test_lib:
	get_specific_reportprogress_info(CLI_Session, 
					 "timeRemainingBeforeFallback"),
    
    case Seconds of
	[] -> 
	    Seconds = "0";
	_ ->
	    Seconds
    end.
    %% Seconds.

%%%--------------------------------------------------------------------
%%% Description: 
%%%--------------------------------------------------------------------
wait_for_time_remaining_before_fallback(CLI_Session, CheckSec) ->
    wait_for_time_remaining_before_fallback(CLI_Session, CheckSec, 3600).

wait_for_time_remaining_before_fallback(_CLI_Session, _CheckSec, Timeout) when Timeout < 0 ->
    ct:fail(" check remaining time for fallback not recieved "
	    "within expected time.");
    
wait_for_time_remaining_before_fallback(CLI_Session, CheckSec, Timeout) ->
    Sec = get_time_remaining_before_fallback(CLI_Session),
    RemainingSec = list_to_integer(Sec),
    %% ct:pal("remaining Sec before fallback allarm: ~p", [RemainingSec]),
    case RemainingSec of
	%% Val when Val > CheckSec-5, Val =< CheckSec ->
	Val when Val > 0, Val =< CheckSec ->
	    ct:pal("## remaining Sec before fallback allarm: ~p", 
		   [RemainingSec]),
	    ok;
	 _ ->
	    timer:sleep(1000),
	    wait_for_time_remaining_before_fallback(CLI_Session, CheckSec, 
						    Timeout - 1)
    end.

%%%--------------------------------------------------------------------
%%% Description: 
%%%--------------------------------------------------------------------
%%cli_connect(CLI_Session)->
%%    ok = rct_cli:connect(CLI_Session).
%%cli_disconnect(CLI_Session)->
%%    ok = rct_cli:disconnect(CLI_Session).

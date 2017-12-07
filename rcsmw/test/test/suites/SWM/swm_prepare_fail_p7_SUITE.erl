%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_prepare_fail_p7_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R5A/R6A/R7A/1
%%% 
%%% @doc == Test Suite for Upgrade Mechanism, perform cancel during prepare phase.  ==
%%% <br/><br/>
%%% @end

-module(swm_prepare_fail_p7_SUITE).
-vsn('/main/R2A/R3A/R5A/R6A/R7A/1').

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
%%% R2A/2      2014-03-03 etxivri     Created
%%% R2A/3      2014-03-25 etxivri     Due to TR HS43738, this errol log filter
%%%                                   has been updated.
%%% R2A/4      2014-04-14 etxivri     Update due to TR HS43738 is solved,
%%% R2A/5      2014-04-15 etxivri     Remove all filter of ERROR logs
%%% R2A/6      2014-06-12 etxivri     Update due to swm changes of reportprogr.
%%% R3A/1      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/2      2015-05-28 etxkols     Changed rct_netconf hook format 
%%% R5A/1      2016-03-16 etxivri     Update due to change behaviour.   
%%% R5A/3      2016-03-16 etxivri     Update due to change behaviour.
%%% R5A/4      2016-05-12 etxnnor     Added check if prepare is in a good state for cancelling
%%% R5A/5      2016-05-13 etxnnor     Minor correction of previous change
%%% R6A/1      2016-06-09 etxivri     Update due to new beaviour.
%%% R7A/1      2016-09-06 etxivri     Changed random to rand due to random is
%%%                                   depricated in OTP19.
%%% ----------------------------------------------------------

%-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).
-export([cancel_during_prepare/1]).

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
		 {rct_core,[]},
                 {rct_logging, {upgrade, 
				[{erlang,{["ERROR REPORT",
					   "CRASH REPORT"],
					  [
					   %% "throw: cancelled"
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
    [cancel_during_prepare].

%%%--------------------------------------------------------------------
%%% @doc 
%%% During prepare the node will reboot unexpected.
%%% - Reboot node when progressPercentage has passed 50%.
%%% <br/><br/>
%%% @spec cancel_during_prepare(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
cancel_during_prepare(_Config) ->
    ct:pal("UG, perform cancel during prepare phase.",[]),

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

    %%%%
    %% Prepares an upgrade package, 
    %% which means downloading a complete UP
    %%%%
    %% No new action id when doing cancel.
    %% Therefore get action id is done here.
    %% OldActionId = swm_test_lib:get_action_id(?NC_Session, MeId),
    OldActionId = no_check,
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					prepare, 
    					Label, 
    					MeId),
    wait_for_prepare_to_be_runed(?CLI_Session, MeId, Label),
    Timer = get_random_number(8),
    ct:pal("Sleep: ~p sec before trig cancel", [Timer]),
    ct:sleep({seconds, Timer}),


    %%%%
    %% Cancel during prepare.
    %%%%
    ct:pal("### cancel",[]),
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					cancel, 
    					Label, 
    					MeId),

    ct:pal("Wait for progress after cancel.", []),
    case swm_test_lib:
    	wait_for_expecting_state(?NC_Session, MeId, OldActionId, "CANCELLED", Label) of
    	%% [{"NOT_AVAILABLE" = Result,
	[{"FAILURE" = Result,
    	  "prepare" = ActionName, 
    	  ProgressInfo, 
    	  ResultInfo, 
    	  "CANCELLED" = State, _ProgReport}] ->
    	    ct:log("result:~p~n"
    		   "actionName:~p~n"
    		   "progressInfo:~p~n"
    		   "resultInfo:~p~n"
    		   "state:~p~n",[Result,
    				 ActionName, 
    				 ProgressInfo, 
    				 ResultInfo,
    				 State]),
    	    ok;
    	Result ->
    	    ct:pal("createUpgradePackage: ~p",[Result]),
    	    ct:fail(Result)
    end,

    %%%%
    %% Remove the created up.
    %%%%
    ct:pal("Remove upgrade package:  ~s",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_Session, "SUCCESS", MeId, Label),

    ok.

%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------

%%%--------------------------------------------------------------------
%%% Description: 
%%%--------------------------------------------------------------------
wait_for_prepare_to_be_runed(CLI_Session, MeId, UP_Label) ->
    wait_for_prepare_to_be_runed(CLI_Session, MeId, UP_Label, 360).

wait_for_prepare_to_be_runed(_CLI_Session, _MeId, _UP_Label, Timeout) 
  when Timeout < 0 ->
    ct:fail(" prepare not runed within expected time.");
    
wait_for_prepare_to_be_runed(CLI_Session, MeId, UP_Label, Timeout) ->
    case is_downloading(CLI_Session, UP_Label) of
	 true ->
	    ct:pal("## progressInfo Downloading found",  []),
	    ok;	
	 false ->
	    timer:sleep(1000),
	    wait_for_prepare_to_be_runed(CLI_Session, MeId, UP_Label, 
					 Timeout - 1)
    end.

%%% ===========================================================================
%%% Description: Check if prepare is in a good state for cancelling
%%% ===========================================================================
is_downloading(CLI_Session, UP_Label) ->
    rct_cli:connect(CLI_Session),
    {ok ,RecievedData} =
	rct_cli:send(CLI_Session,
		     "show ManagedElement=1,SystemFunctions=1,SwM=1,UpgradePackage=" ++ UP_Label),
    rct_cli:disconnect(CLI_Session),

    Data = string:tokens(RecievedData, "=\r\n \""),
    ct:log("show : ~p", [Data]),
    ct:log("flatten list : ~p", [lists:flatten(Data)]),
    case re:run(lists:flatten(Data),"progressInfoDownloading",[]) of
	{match, _} ->true;
	nomatch -> false;
	Other -> ct:pal("cancelling not possible, show data: ~p", [Other]),
		 false
    end.


get_random_number(MaxNr) ->
    %%%%
    %% Get a random nr.
    %%%%
    RandomNr = rand:uniform(MaxNr),
    ct:pal("TC will use Random nr: ~p",[RandomNr]),
    RandomNr.

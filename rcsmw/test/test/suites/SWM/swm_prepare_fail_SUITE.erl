%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_prepare_fail_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2013-2015
%%% @version /main/R2A/R3A/3
%%% 
%%% @doc == Test Suite for Upgrade Mechanism when prepare fails. ==
%%% <br/><br/>
%%% @end

-module(swm_prepare_fail_SUITE).
-vsn('/main/R2A/R3A/3').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2015 All rights reserved.
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
%%% R2A/2      2013-12-13 etxivri     Created
%%%                                   Only possible to run TC on TCU.
%%%                                   Since fallocate works ext4 fileformat,
%%%                                   Not on ext3.
%%% R2A/3      2014-01-10 etxivri     Added create create of cxps used for UG.
%%%                                   This is runed in init_per_suite.
%%% R2A/4      2014-01-15 etxivri     Changed some ct:pal to ct:log
%%% R2A/5      2014-01-16 etxivri     Update due to changes in swm_test_lib
%%% R2A/5      2014-01-16 etxivri     Check used disc when prepare fail.
%%% R2A/7      2014-01-27 etxivri     Use lib function to build UP.
%%% R2A/8      2014-02-06 etxivri     Minor update in ct:pal.
%%%                                   Add rct_upgrade hook name when create up.
%%% R2A/9      2014-03-25 etxivri     Updated error log filter.
%%% R2A/9      2014-04-14 etxivri     Update error log filter and 
%%%                                   check additional info
%%% R2A/11     2014-06-12 etxivri     Update due to swm changes of reportprogr.
%%% R2A/12     2014-06-25 etxivri     Update to create large fil in /rcs/ path.
%%% R2A/13     2014-07-09 etxivri     Update of exp info
%%% R2A/13     2014-08-05 etxivri     Update due to handling when disc is ful.
%%% R2A/15     2014-09-18 etxivri     Updated error log filter and result_info
%%% R2A/16     2014-10-01 etxivri     Updated error log filter and result_info
%%% R3A/1      2014-10-16 etxivri     Updated error log filter and result_info
%%% R3A/2      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/3      2015-05-28 etxkols     Changed rct_netconf hook format 
%%% ----------------------------------------------------------

%-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).
-export([prepare_when_disc_is_full/1]).

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).
-define(NC_Session, nc1).

-define(PATH, "/rcs/").
%% -define(PATH, "/home/sirpa/dev_patches/").

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
					   "CRASH REPORT"
					  ],
					  [
					   %% "fail,\"Disk space shortage"
					   "fail,\"Download failed due to disk space shortage"
					   %% "fail,\"no space left on device\""
					   %%"no space left on device"
					  ]
					 }}]}},
		 {rct_netconf,nc1}]}].

%% @hidden
init_per_suite(Config) ->
    ct:log("# init per suite. ~n"
    	   "Create cxps that shall be used for UG."), 
    swm_test_lib:build_valid_ug_packakage(?NC_Session, ug1),
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
	    rct_rpc:call(rpc_1, os, cmd, 
			 ["rm "++?PATH++"large_test_file*"], 
			 10000, noprint),
	    rct_rpc:call(rpc_1, os, cmd, 
			 ["df"], 
			 10000, noprint),
	    
    %%% Remove created upgrade package.
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
    [prepare_when_disc_is_full].

%%%--------------------------------------------------------------------
%%% @doc 
%%% Prepare an upgrade package when disc is full. 
%%% The result shall be FAILURE.
%%% @end
%%%--------------------------------------------------------------------
prepare_when_disc_is_full(_Config) ->
    ct:pal("UG prepare fails due to full disc.",[]),
    Up = rct_rpc:call(rpc_1, swmI, get_current_up_metadata, [], 10000, noprint),
    ct:pal("Current UP:~n~p~n",[Up]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Create UG
    %%%%
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:log("UGPath:~n~p~n",[UGPath]),
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

    {AvailableDisc, UsedDisc} = get_disc_usage(),
    ct:pal("Disc usage before filling the disc.~n"
	   "AvailableDisc: ~p ~n"
	   "UsedDisc: ~p%", [AvailableDisc, UsedDisc]),
    %%%%
    %% Fill up the disc
    %%%%
    ok = fill_disc(),
    {AvailableDisc1, UsedDisc1} = get_disc_usage(),
    ct:pal("Disc usage after filling the disc.~n"
	   "AvailableDisc: ~p ~n"
	   "UsedDisc: ~p%", [AvailableDisc1, UsedDisc1]),

    %%%%
    %% Prepares an upgrade package, which means downloading a complete UP
    %% This will fail due to dis full
    %%%%
    ActionId_2 = swm_test_lib:get_action_id(?NC_Session, MeId),
    %% ActionId_2 = get_action_id(?NC_Session, MeId),
    ok = swm_test_lib:up_action_generic(?NC_Session, 
					prepare, 
					Label, 
					MeId),

    ct:pal("Wait for progress after prepare UP that will fail."),
    case swm_test_lib:
	wait_for_progress_result(?NC_Session, MeId, ActionId_2) of
	[{"FAILURE" = A, 
	  "prepare"= B, 
	  "The action could not be completed" = _C,
	  D,
	  State1, 
	  ProgReport1}] ->
	    ct:log("ProgReport: ~p ~n", [ProgReport1]),
	    ct:pal("actionName: ~p~n"
		   "state: ~p~n"
		   "resultInfo: ~p~n"
		   "result:~p~n", [B, State1, D, A]),
	    ok;
	PrepResult ->
	    D = dummy,
	    ct:pal("~p: ~p",[prepare, PrepResult]),
    	    ct:fail(PrepResult)
    end,

    {AvailableDisc2, UsedDisc2} = get_disc_usage(),
    ct:pal("Disc usage after prepare fail.~n"
	   "AvailableDisc: ~p ~n"
	   "UsedDisc: ~p%", [AvailableDisc2, UsedDisc2]),

    %% Check that swm clean up used memory when UG failure due to full disk.
    %% AvailableDisc shall not be 0.
    case list_to_integer(AvailableDisc2) of
	Val when Val < 300 ->
	    ct:pal("TC Shall fail due to AvailableDisc is to low after UG fail."
		   "Used disc should be released!",[]);
	    %% ct:fail("TC will fail due to AvailableDisc shall not be 0 ");
	_ ->
	    ok
    end,

    %%%%
    %% Remove the large file
    %%%%
    [] = rct_rpc:call(rpc_1, os, cmd, 
		      ["rm "?PATH++"large_test_file*"], 
		      10000, noprint),

    {AvailableDisc3, UsedDisc3} = get_disc_usage(),
    ct:pal("Disc usage after removing the large test file.~n"
	   "AvailableDisc: ~p ~n"
	   "UsedDisc: ~p%", [AvailableDisc3, UsedDisc3]),

    %%%%
    %% Remove the created up.
    %%%%
    ct:pal("Remove upgrade package: : ~s",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_Session,
					"SUCCESS",
					MeId,
					Label),

    %%%%
    %% Check exp str exist in result info
    %%%%
    {match, _ } = re:run(D, "disk space shortage"),

    ok.


%%%--------------------------------------------------------------------
%%% Internal Functions
%%%--------------------------------------------------------------------

%%%--------------------------------------------------------------------
%%% Description: Fill up the disc to atleast 98 %.
%%%--------------------------------------------------------------------
fill_disc() ->
    {AvailableDisc, _DiscUsage} = get_disc_usage(),
    ct:pal("AvailableDisc: ~p",[AvailableDisc]),
    
    %%%%
    %% Create a file using the remaing file size of disc.
    %%%%
    %% %% in bytes
    [] = rct_rpc:call(rpc_1, os, cmd, 
		     ["fallocate -l "++AvailableDisc++"000" ++ 
			  " "++?PATH++"large_test_file"], 
		     30000, noprint),

    check_disc_is_full(),

    ok.
    
check_disc_is_full() ->
    timer:sleep(5000), %% Need if use fallocate again.
    {AvailableDisc, DiscUsage} = get_disc_usage(),
    
    case DiscUsage of
	Val when Val > "97" ->
	    ct:log("Val: ~p%",[Val]),
	    ct:log("UsedDisc: ~p%",[DiscUsage]),
	    ok;
	_Val ->
	    ct:pal("Increase DiscUsage due to DiscUsage is only: ~p ~n"
		   "Create a new file with remaining size.", [_Val]),
	    [] = rct_rpc:call(rpc_1, os, cmd, 
			      ["fallocate -l "++AvailableDisc++"000" ++ 
				   " "++?PATH++"large_test_file1"], 
			      30000, noprint)
	    %% get_disc_usage()
    end,
	    
    ok.

%%%--------------------------------------------------------------------
%%% Description: Get disc usage.
%%%--------------------------------------------------------------------
get_disc_usage() ->
    DF = rct_rpc:call(rpc_1, os, cmd, ["df /rcs"], 10000, noprint),
    ct:log("~p",[DF]),

    DF_ElemList = string:tokens(DF, " %/ \n"),
    ct:log("~p",[DF_ElemList]),
    
    ReversedList = lists:reverse(DF_ElemList),
    ct:log("~p",[ReversedList]),
    
    ["rcs", UsedDisc, AvailableDisc | _] = ReversedList,
    ct:log("# AvailableDisc: ~p",[AvailableDisc]),
    ct:log("# UsedDisc: ~p",[UsedDisc]),

    {AvailableDisc, UsedDisc}.

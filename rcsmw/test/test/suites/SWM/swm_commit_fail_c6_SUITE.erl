%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_commit_fail_c6_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R2A/R3A/2
%%% 
%%% @doc == Test Suite for Upgrade Mechanism, confirm fails due to prepare phase not done.  ==
%%% <br/><br/>
%%% @end

-module(swm_commit_fail_c6_SUITE).
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
%%% R2A/2      2014-02-03 etxivri     Created
%%% R2A/3      2014-05-22 erarafo     Partly adapted due to HS62272
%%% R2A/4      2014-07-09 etxivri     Update ERROR log filter.
%%% R2A/5      2014-08-12 etxivri     Update due to changed behaviour.
%%% R3A/1      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/2      2015-05-28 etxkols     Changed rct_netconf hook format 
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
-export([commit_fail_due_prepare_not_done/1]).

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
					  ["Confirm can not be executed in state INITIALIZED"]
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
    [commit_fail_due_prepare_not_done].

%%%--------------------------------------------------------------------
%%% @doc 
%%% Confirm will fail if prepare is not done.
%%% <br/><br/>
%%% @spec commit_fail_due_prepare_not_done(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
commit_fail_due_prepare_not_done(_Config) ->
    ct:pal("UG, commit fails due to prepare phase not done",[]),

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
    %% Commit upgrade package when prepare not done.
    %%%%    
    Action =  {'ManagedElement',
    	       [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
    	       [{managedElementId, [], [MeId]},
    		{'SystemFunctions',
    		 [{systemFunctionsId,[],["1"]},
    		  {'SwM',
    		   [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
    		   [{swMId,[],["1"]},
		    {'UpgradePackage', [],
		     [{upgradePackageId, [Label]},
		      {confirm, [], []}]}]}]}]},

    ct_netconfc:open(?NC_Session, []),
    {error, Res} = ct_netconfc:action(?NC_Session, Action),
    ct:log("Res:~n~p~n",[Res]),
    ct_netconfc:close_session(?NC_Session),

    {'error-message', _, ErrMess} = lists:keyfind('error-message', 1, Res),
    ct:log("ErrMess:~n~p~n",[ErrMess]),
    
    %%%%
    %% Check err info
    %%%%
    Flatten = lists:flatten(ErrMess),
    ErrStr = string:tokens(Flatten,"[]"),
    ErrInfoStr = lists:last(ErrStr),
    ct:log("ErrInfoStr:~n~p~n",[ErrInfoStr]),
    "Confirm can not be executed in state INITIALIZED" = ErrInfoStr,

    %%%%
    %% Check up state is still in INITIALIZED
    %%%%
    swm_test_lib:wait_for_exp_up_state(?NC_Session, 
				       MeId, 
				       Label, 
				       "INITIALIZED"),

    %%%%
    %% Remove the created up.
    %%%%
    ct:pal("Remove upgrade package: : ~s",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_Session, "SUCCESS", MeId, Label),

    ok.

%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------

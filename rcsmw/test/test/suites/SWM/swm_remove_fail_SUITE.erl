%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_remove_fail_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R4A/R6A/R8A/1
%%%
%%% @doc == Test Suite when differents remove is used that shall result in fail. ==
%%% <br/><br/>
%%% @end

-module(swm_remove_fail_SUITE).
-vsn('/main/R2A/R3A/R4A/R6A/R8A/1').

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
%%% R2A/2      2014-05-14 etxivri     Created
%%% R2A/3      2014-05-15 etxivri     Added E10
%%% R2A/4      2014-05-15 etxivri     Update remove sw version
%%% R2A/5      2014-05-16 etxivri     Minor update.
%%% R2A/6      2014-05-23 etxivri     Changed commit to confirm.
%%% R2A/7      2014-06-04 etxivri     unsucc_rem_up_used_by_bu is now ok to run.
%%% R2A/8      2014-06-11 etxivri     Update matching str
%%% R2A/9      2014-06-12 etxivri     Update due to swm changes of reportprogr.
%%% R2A/10     2014-07-11 etxivri     Update exp resultInfo in unsucc_rem_act_
%%% R3A/1      2014-10-17 etxivri     Update due to new behaviour on backups.
%%% R3A/2      2014-10-21 etxivri     Bugfix.
%%% R3A/3      2015-01-29 etxivri     Update to remove error in ct-shell.
%%% R4A/1      2015-05-20 erarafo     Adapted to model change
%%% R4A/2      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R4A/3      2015-06-03 etxkols     Cluster fix
%%% R6A/1      2016-05-27 etxkols     Make it more robust in 17A
%%% R8A/1      2016-11-23 etxkivri    Update to handle several BUs.     
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
-export([precondition_bu_ug/1,
	 unsucc_rem_of_active_up/1,
	 unsucc_rem_up_used_by_bu/1,
	 unsucc_rem_act_sw_ver/1
	]).

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).
-define(NC_Session, nc1).
-define(CLI_Session, cli1).

-define(BU_Name_1, "bu_test_1").

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
		 {rct_coli, {coli, [manual_connect, {connect_timeout, 60}]}},
                 {rct_logging, {upgrade,
				[{erlang,{["ERROR REPORT",
					   "CRASH REPORT"],
					  ["swmServer: throw {active"]
					 }}]}},
		 {rct_cli, {cli1, [manual_connect]}},
		 {rct_netconf,nc1}]}].


%% @hidden
init_per_suite(Config) ->
    swm_test_lib:set_housekeeping_delay_variable(rpc_1),
    Config.

%% @hidden
end_per_suite(_Config) ->
    swm_test_lib:erase_housekeeping_delay_variable(rpc_1),
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
	    ct:pal("Testcase failed due to: ~p.  \n", [Reason])
    	    %% 	   "Clean up! TBD!", [Reason]),
      	    %% ct:pal("Testcase failed due to: ~p.  \n"
    	    %% 	   "Clean up! TBD!", [Reason]),
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
    [precondition_bu_ug, 
     unsucc_rem_of_active_up, 
     unsucc_rem_up_used_by_bu,
     unsucc_rem_act_sw_ver
    ].

%%%--------------------------------------------------------------------
%%% @doc
%%% As precondition, Perform an correct upgrade <br/>
%%% @spec precondition_bu_ug(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
precondition_bu_ug(_Config) ->
    ct:pal("As precondition, Perform a backup then perform a "
	   "correct upgrade. No failure."),

    ct:log("# init per suite build valid to up. ~n"
    	   "Create cxps that shall be used for UG."),
    swm_test_lib:build_valid_ug_packakage(?NC_Session),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Create backup, will be used in TC NodeUC416:E9
    %%%%
    ActionId = swm_br_lib:get_action_id(?NC_Session, MeId),
    swm_br_lib:create_backup(?NC_Session, 
			     MeId, 
			     ?BU_Name_1),
    case swm_br_lib:
	wait_for_expecting_state(?NC_Session, MeId, ActionId, "FINISHED") of
	[{"SUCCESS", 
	  _ActionName, 
	  _ProgressInfo, 
	  _ResultInfo, 
	  _State, 
	  ProgressReport}] ->
	    ct:pal("Progress: ~p~n",[ProgressReport]),
	    ok;
	Result ->
	    ct:pal("createBackup: ~p~n",[Result]),
	    ct:fail(Result)
    end,

    %%%%
    %% Perform upgrade.
    %%%%
    create(MeId),
    
    perform_ug_action(prepare),

    perform_ug_action(verify),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    net_kernel:disconnect(ErlNode),
    perform_ug_action(activate),

    perform_ug_action(confirm),

    ok.

%%% ===========================================================================
%%% @doc
%%% NodeUC416.E8: <br/>
%%% Unsuccessful removal of active Upgrade Package. <br/>
%%% @spec unsucc_rem_of_active_up(_Config) -> ok
%%% @end
%%% ===========================================================================
unsucc_rem_of_active_up(_Config) ->
    ct:pal("NodeUC416.E8:Unsuccessful removal of active Upgrade Package"),

    %%% Run perform_upgrade before run this TC.

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),
 
    %%%%
    %% Get Latest UP
    %%%%
    Label = get_latest_up(),

    %%%%
    %% Remove active up. Shall result in failure
    %%%%
    ct:pal("Remove active upgrade package: : ~s",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_Session,
    					"FAILURE",
    					MeId,
    					Label),
    Report = swm_test_lib:get_swm_report(?NC_Session, MeId),
    ct:pal("Result from remove active upgrade package:~p",[Report]),
    
    %%%%
    %% Check exp fail string is shown in resultinfo.
    %%%%
    ExpStr = "This upgrade package contains active software. "
	"It cannot be removed at this time.",
    [ {resultInfo,[], [ExpStr] }] =  
	 [X || {resultInfo, _ , _} = X <- Report],

    ok.

%%% ===========================================================================
%%% @doc
%%% NodeUC416.E9: <br/>
%%% Unsuccessful removal of used Upgrade Package, <br/>
%%% since used by existing BU. <br/>
%%% @spec unsucc_rem_up_used_by_bu(_Config) -> ok
%%% @end
%%% ===========================================================================
unsucc_rem_up_used_by_bu(_Config) ->
    ct:pal("NodeUC416.E9:Unsuccessful removal of used Upgrade Package, "
	   "since used by existing BU"),

    %%% Run create_backup, perform_upgrade before run this TC.

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),
 
    %%%%
    %% Get software version that is used in BU.
    %%%%
    Backups = swm_br_lib:get_all_backups(?NC_Session, MeId),
    ct:pal("Backups: ~p ",[Backups]),

    BuId = get_correct_buid(?BU_Name_1, Backups),
    ct:pal("Selected backup ~p.~n",[BuId]),

    BuSwVer = get_bu_sw_ver(?NC_Session, MeId, BuId),
    ct:pal("BuSwVer: ~p.~n",[BuSwVer]),

    %%%%
    %% Remove active up. Shall result in failure
    %%%%
    ct:pal("Remove active upgrade package: : ~s",[BuSwVer]),
    swm_test_lib:remove_upgrade_package(?NC_Session,
    					"FAILURE",
    					MeId,
    					BuSwVer),
    Report = swm_test_lib:get_swm_report(?NC_Session, MeId),
    ct:pal("Result from remove active upgrade package:~p",[Report]),
    
    %%%%
    %% Check exp fail string is shown in resultinfo.
    %%%%
    ExpStr = "This upgrade package is referred by a backup. "
	"It cannot be removed at this time.",
    [ {resultInfo,[], [ExpStr] }] =  
    	 [X || {resultInfo, _ , _} = X <- Report],

    ok.


%%% ===========================================================================
%%% @doc
%%% NodeUC416.E10: <br/>
%%% Unsuccessful removal of a Software Version, since active<br/>
%%% @spec unsucc_rem_act_sw_ver(_Config) -> ok
%%% @end
%%% ===========================================================================
unsucc_rem_act_sw_ver(_Config) ->
    ct:pal("NodeUC416.E10: Unsuccessful removal of a Software Version, "
	   "since active"),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session), 
    ct:pal("+++ MeId: ~p", [MeId]),
    
    %%%%
    %% Get active software version.
    %%%%
    %% SwVersion = swm_test_lib:get_sw_version(?NC_Session, MeId),
    UPs = swm_test_lib:get_ups(?NC_Session),
    ct:pal("UPs:~n~p~n",[UPs]),
    SwVersion = swm_test_lib:get_highest_label(UPs),
    ct:pal("Active SwVersion to be removed: ~p", [SwVersion]),

    remove_sw_version(?NC_Session, MeId, SwVersion),

    wait_for_state_finished(MeId),

    Report = swm_test_lib:get_swm_report(?NC_Session, MeId),
    ct:pal("Result from remove active SwVersion:~p", [Report]),

    %%%%
    %% Check exp fail result and string is shown in report.
    %%%%
    [ {state,[], ["FINISHED"] }] =  
    	 [A || {state, _ , _} = A <- Report],
        [ {result,[], ["FAILURE"] }] =  
    	 [B || {result, _ , _} = B <- Report],
    %% ExpStr = "Cannot remove active version!",
    ExpStr = "Cannot remove active version "++SwVersion,
    [ {resultInfo,[], [ExpStr] }] =  
    	 [X || {resultInfo, _ , _} = X <- Report],

    ok.
     
wait_for_state_finished(MeId) ->
    wait_for_state_finished(MeId, 60000).
wait_for_state_finished(_MeId, Timeout) when Timeout < 0 ->
    ct:fail("State not in FINISHED within exp time.");
wait_for_state_finished(MeId, Timeout) ->
    Report = swm_test_lib:get_swm_report(?NC_Session, MeId),
    ct:log("Result from remove active SwVersion:~p", [Report]),
    case [A || {state, _ , _} = A <- Report] of
	[ {state,[], ["FINISHED"] }] ->
	    ct:pal("# State is in FINSHED");
	_Other ->
	    timer:sleep(5000),
	    wait_for_state_finished(MeId, Timeout-5000)
    end.


%%%--------------------------------------------------------------------
%%% @doc
%%% Create. <br/>
%%% @spec create(MeId) -> ok
%%% @end
%%%--------------------------------------------------------------------
create(MeId) ->
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
    ok.



%% %%%--------------------------------------------------------------------
%% %%% @doc
%% %%% remove. <br/>
%% %%% @spec remove(_Config) -> ok
%% %%% @end
%% %%%--------------------------------------------------------------------
%% remove(_Config) ->
%%     %%%%
%%     %% Get managedElementId
%%     %%%%
%%     MeId = swm_test_lib:get_me_id(?NC_Session),

%%     %%%%
%%     %% Get Latest UP
%%     %%%%
%%     Label = get_latest_up(),

%%     %%%%
%%     %% Remove the created up.
%%     %%%%
%%     ct:pal("Remove upgrade package: : ~s",[Label]),
%%     swm_test_lib:remove_upgrade_package(?NC_Session,
%% 					"SUCCESS",
%% 					MeId,
%% 					Label),

%%     ok.

%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------
get_latest_up() ->
    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    ct:pal("Label:~n~p~n",[Label]),
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

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
get_correct_buid(BuName, AllBackUps) ->
    List = lists:dropwhile(fun({_BrmBackup, _, Data}) ->
				   case lists:keysearch(backupName, 1, Data) of
				       {value,{backupName,[],[BuName]}} ->
				   	   false;
				       _ ->
				   	   true
				   end
			   end, AllBackUps),
    ct:pal("BuIdList: ~p", [List]),
    [WantedList | _] = List,
    {_BrmBackup, _, WantedData} = WantedList,
    {value,{brmBackupId,[],[BuId]}} = 
	lists:keysearch(brmBackupId, 1, WantedData),
    [BuId].

%%%--------------------------------------------------------------------
%%% @doc
%%% Get software version that is used by the backup.
%%% @end
%%%--------------------------------------------------------------------
get_bu_sw_ver(NC_Session, MeId, BuId) ->
    Get = {'ManagedElement',
	   [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId, [], [MeId]},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'BrM',
	       [{brMId,[],["1"]},
		{'BrmBackupManager', 
		 [{brmBackupManagerId,[],["1"]},
		 {'BrmBackup',[],
		  [{brmBackupId,[],BuId},
		   {swVersion,[]}
		   %% {swVersion,[{struct,"ProductData"}], []}
		  ]}]
		}]}]}]},
    
    {ok, _} = ct_netconfc:open(NC_Session, []),

    {ok, Res} =ct_netconfc:get(NC_Session, Get),

    ok = ct_netconfc:close_session(NC_Session),

    ct:pal("# Res: ~p", [Res]),

    [{'ManagedElement',
      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
      [{managedElementId,[],["1"]},
       {'SystemFunctions',[],
    	[{systemFunctionsId,[],["1"]},
    	 {'BrM',
    	  [{xmlns,"urn:com:ericsson:ecim:RcsBrM"}],
    	  [{brMId,[],["1"]},
    	   {'BrmBackupManager',[],
    	    [{brmBackupManagerId,[],["1"]},
    	     {'BrmBackup',[],
    	      [{brmBackupId,[],BuId},
	       {swVersion,
		[{struct,"ProductData"}],
		SwVersionData}
	      ]}]}]}]}]}] = Res,

    {value,{productNumber,[],[CXS]}} = 
	lists:keysearch(productNumber, 1, SwVersionData),
    {value,{productRevision,[],[Label]}} = 
	lists:keysearch(productRevision, 1, SwVersionData),

    CxsLabel = CXS++"-"++Label,
    CxsLabel.

%%%--------------------------------------------------------------------
%%% @doc Remove a software version. The version is specified here as
%%% the value part of the SwVersion MO RDN.
%%% 
%%% The Result below will contain {returnValue,[],["true"]} even if
%%% an attempt is made to remove the active software version. This is
%%% as it should be according to etxjotj.
%%% @end
%%%--------------------------------------------------------------------
remove_sw_version(NC_Session, MeId, SwVersionRdnValue) ->
    SwInventory = "ManagedElement=1,SystemFunctions=1,SwInventory=1",
    SwVersion = SwInventory++",SwVersion="++SwVersionRdnValue,
    
    {ok, _} = ct_netconfc:open(NC_Session, []),
    
    NetconfAction =
	{'ManagedElement',
	 [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId, [], [MeId]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'SwM',
	     [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
	     [{swMId,[],["1"]},
	      {removeSoftwareVersion, [],
	       [{swVersion,[],
		 [SwVersion]} ]}]}]}]},
    % ct:pal("NETCONF action: ~p", [NetconfAction]),

    NetconfActionResult = ct_netconfc:action(NC_Session, NetconfAction),

    try NetconfActionResult of
	{ok, Result} ->
	    ct:pal("Remove SwVersion: ~p", [Result]);
	_ ->
	    ct:fail("unexpected result: ~p", [NetconfActionResult])
    catch
	_:_ ->
	    can_not_happen
    after
	ok = ct_netconfc:close_session(NC_Session)
    end,
    ok.

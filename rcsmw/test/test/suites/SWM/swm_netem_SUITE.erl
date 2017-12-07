%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_netem_SUITE.erl %
%%% @author etxnnor
%%% @copyright Ericsson AB 2016
%%% @version /main/R5A/R6A/2
%%% 
%%% @doc == TestSuite for testing import/export operations during bad network conditions simulated with the netem tool.==
%%% <br/><br/>
%%% @end

-module(swm_netem_SUITE).
-vsn('/main/R5A/R6A/2').
-author('etxnnor').
-include_lib("common_test/include/ct.hrl").

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016 All rights reserved.
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
%%% Rev        Date        Name        What
%%% -----      ---------   --------    ------------------------
%%% R5A/1      2016-04-04  etxnnor     Created
%%% R5A/2      2016-04-13  etxnnor     Updated
%%% R5A/3      2016-05-31  etxnnor     Update in init_per_suite to when get release,
%%%                                    now it will accept even if no value is set in
%%%                                    field (same as swm_SUITE.erl)
%%% R6A/1      2016-08-18  etxkols     Git migration requires that CC paths is not used 
%%% R6A/2      2016-08-26  etxnnor     Fix due to change in SWM (empty progressReport)
%%% ----------------------------------------------------------
%%% 

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 test_prepare_netem/1,
	 test_export_backup/1,
	 test_export_esi/1]).

-define(NC_Session, nc1).
-define(CLI_Session, cli1).
-define(Sleep, 1000).
-define(DataDir, rct_cc_git_path:find("RCT_TOP", ["test/suites/SWM/swm_backup_SUITE_data"])).
-define(BackupName, "swm_netem_SUITE").

suite() -> 
    case check_if_vc_board() of
	"yes" -> [{timetrap, {hours, 4}},
		  {ct_hooks, [{rct_htmllink,[]},
			      {rct_cli, {cli1,
					 [{user, "SysAdminTest"}, 
					  {password, "SysAdminTest"},
					  manual_connect]}},
			      {rct_upgrade,ug1},
			      {rct_netconf, {nc1, man_auth}},
			      {rct_rs232,console},
			      {cth_conn_log,[]},
			      {rct_ssh, {netem, [{ip_type, ssh_netem}, manual_connect]}}
			     ]}];
	_->
	    [{timetrap, {hours, 4}},
	     {ct_hooks, [{rct_htmllink,[]},
			 {rct_rpc, rpc_1},
			 {rct_upgrade,ug1},
			 {rct_logging, 
			  {upgrade, 
			   [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
			 {rct_netconf,nc1},
			 {cth_conn_log,[]},
			 {rct_ssh, {netem, [{ip_type, ssh_netem}, manual_connect]}},
			 {rct_rs232,console}
			]}]
    end.

init_per_suite(Config) ->
    ok = set_netem_delay(),
    {ok, Reply} = check_netem(),
    Re_match_string = "eth2\\sroot\\srefcnt\\s2\\slimit\\s10000\\sdelay\\s.*\\n.*dev\\seth3\\sroot\\srefcnt\\s2\\slimit\\s10000\\sdelay\\s.*",
    case re:run(Reply, Re_match_string) of
    	{match, _} -> ct:pal("MATCH!");
        nomatch -> ct:pal("nomatch")
    end,

    MeId = case check_if_vc_board() of
	       "yes" -> get_me_id(?NC_Session);
	       _->
		   MeData = rct_rpc:call(rpc_1, comsaI, 
					 get_managed_element_data, [], 10000, 
					 noprint),
		   {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
		   net_kernel:disconnect(ErlNode),
		   case proplists:get_value(networkManagedElementId, MeData) of
		       undefined ->
			   "1";
		       NMEI -> NMEI
		   end
	   end,
    Get = {'ManagedElement', 
	   [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId, [], [MeId]},
	    {release, []},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'SwM',
	       [],
	       [{swMId,[],["1"]}]}]}]},

    RootDir = proplists:get_value(priv_dir, Config),
    os:cmd(["chmod a+rwx ", RootDir]),

    {ok, R} = netconf(get, [nc1, Get]),
    {ok, {_, _, Contents}} = extract_element('SwM', R),
    ct:pal("Current packages:~n"++
      [begin
	   {ok, {_, _, [State]}} = extract_element(state, [UPE]),
	   {ok, {_, _, [UpId]}} = extract_element(upgradePackageId, [UPE]),
	   io_lib:format("UpId: ~p State: ~s~n",[UpId, State])
       end||
	  UPE<-Contents,
	  element(1, UPE) == 'UpgradePackage'],[]),
    
%%    {ok, {_, _, [Release]}} = extract_element(release, R),
    Release = case extract_element(release, R) of
    		  {ok, {_, _, [Rel]}} ->
    		      Rel;
    		  {ok,{release,[],[]}} ->  %% BPU has no value in release, yet!
    		      ct:pal("value in release not exist.~n",[]),
    		      ""
    	      end,
    ct:pal("Release = ~p~n",[Release]),

    [{host, SftpHost},{username, Username},{password, Password}] = 
	ct:get_config(sftp_server),
    [{meId, MeId},
     {backupName, ?BackupName},
     {release, Release},
     {sftp_host, SftpHost},
     {sftp_root, RootDir},
     {sftp_user, Username},
     {sftp_pass, Password}| Config].
end_per_suite(_Config) ->
    ok = unset_netem_delay(),
    {ok, Reply2} = check_netem(),
    case re:run(Reply2, "delay") of
	{match, _} -> ct:pal("nok");
        nomatch -> ct:pal("OK!")
    end,
    ok.
init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(_TestCase, _Config) ->
    ok.


%% Test cases
all() -> 
    [test_prepare_netem, test_export_backup, test_export_esi].

test_prepare_netem(Config) ->
    clean_up(Config),
    NewConfig = create(Config),
    prepare(NewConfig),    
    ok.

test_export_backup(Config) ->
    clean_backups(Config),
    export_backup(Config),
    ok.

test_export_esi(Config) ->
    export_esi(Config),
    ok.


%% Helper functions
set_netem_delay() ->
    rct_ssh:connect(netem),
    rct_ssh:exec(netem, "tc qdisc add dev eth2 root netem delay 80ms 2ms limit 10000", 5000, ""),
    rct_ssh:exec(netem, "tc qdisc add dev eth3 root netem delay 80ms 2ms limit 10000", 5000, ""),
    rct_ssh:disconnect(netem),
    ok.

unset_netem_delay() ->
    rct_ssh:connect(netem),
    rct_ssh:exec(netem, "tc qdisc del dev eth2 root netem delay 80ms 2ms limit 10000", 5000, ""),
    rct_ssh:exec(netem, "tc qdisc del dev eth3 root netem delay 80ms 2ms limit 10000", 5000, ""),
    rct_ssh:disconnect(netem),
    ok.

check_netem() ->
    rct_ssh:connect(netem),
    Reply = rct_ssh:exec(netem, "tc qdisc show", 5000, ""),
    rct_ssh:disconnect(netem),
    Reply.


create(Config) ->
    From_Label = swm_test_lib:get_sw_version(?NC_Session),

    MeId = proplists:get_value(meId, Config),
    Host = proplists:get_value(sftp_host, Config),
    User = proplists:get_value(sftp_user, Config),
    Password = proplists:get_value(sftp_pass, Config),
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),

    Uri = "sftp://"++User++"@"++Host++UGPath,

    ProgressFilter =  {'ManagedElement', 
		       [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
		       [{managedElementId, [], [MeId]},
			{'SystemFunctions',
			 [{systemFunctionsId,[],["1"]},
			  {'SwM',
			   [],
			   [{swMId,[],["1"]},
			    {reportProgress, []}]}]}]},
    ActionId = get_action_id(ProgressFilter),
    Start1 = os:timestamp(),
    %% {ok, _} = netconf_open(nc1, []),
    Action =  {'ManagedElement', 
	       [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
	       [{managedElementId, [], [MeId]},
		{'SystemFunctions',
		 [{systemFunctionsId,[],["1"]},
		  {'SwM',
		   [],
		   [{swMId,[],["1"]},
		    {createUpgradePackage, [], 
		     [{uri, [Uri]},
		      {password, [Password]}]
		     }]}]}]},
    {ok, Reply} = netconf(action, [nc1, Action]),
    {ok, {returnValue, _, [ActionResult]}} =
	extract_element(returnValue, Reply),
    ct:pal("Reply: ~p",[ActionResult]),
    case check_if_vc_board() of
	"yes" -> wait_for_swm_progress_done(?NC_Session,
					    "SUCCESS", 
					    "createUpgradePackage",
					    MeId,
					    ActionId);
	_ ->
	    case wait_for_progress(reportProgress, ActionId, ProgressFilter) of
		["SUCCESS"] ->
		    ok;
		Result ->
		    ct:pal("createUpgradePackage: ~s",[Result]),
		    ct:fail(Result)
	    end
    end,
   
    End1 = os:timestamp(),
    UgCreateTime = trunc(timer:now_diff(End1, Start1) /1000/1000),
    ct:pal("UgCreateTime: ~p~n",[UgCreateTime]),

    ReportProgress = get_report_progress(nc1, MeId, ProgressFilter),
    ct:pal("ReportProgress: ~p",[ReportProgress]),

    Label = get_up_package(ReportProgress),

    Release = proplists:get_value(release, Config),
    assert_release(MeId, Release, keep),
	

    %% Add created up label to Config. Add upgrade times. 
    %% Will be used in other upgrade actions.
    NewConfig = [{saved_config, {create,[{from_label, From_Label},
					 {uplabel, Label},
					 {create_time, UgCreateTime}]}}],
    ct:pal("Add NewConfig: ~p",[NewConfig]),
    ct:pal("What is returned: ~p",[lists:flatten([Config, NewConfig])]),
    lists:flatten([Config, NewConfig]).

prepare(Config) ->
    Label = get_latest_up(Config),
    MeId = proplists:get_value(meId, Config),

    UgPrepareTime = case check_if_vc_board() of
			"yes" -> 
			    perform_ug_action(prepare, Config, Label, MeId);
			_ ->   
			   up_action_generic(Config, prepare, Label, MeId)
		    end,
    ct:pal("UgPrepareTime: ~p~n",[UgPrepareTime]).


%%%--------------------------------------------------------------------
%%% @doc Removes all non active upgrade packages
%%% @end
%%%--------------------------------------------------------------------
clean_up(Config) ->
    MeId = proplists:get_value(meId, Config),
 
    Content = get_swm_content(MeId),
    ActiveSwVersion = get_active_sw_version(MeId),

    ProgressFilter =  {'ManagedElement', 
		       [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
		       [{managedElementId, [], [MeId]},
			{'SystemFunctions',
			 [{systemFunctionsId,[],["1"]},
			  {'SwM',
			   [],
			   [{swMId,[],["1"]},
			    {reportProgress, []}]}]}]},
    ActionId = get_action_id(ProgressFilter),


    [case Element of
	 {'UpgradePackage', _, UpE} ->
	     {ok, {upgradePackageId, _, [Id]}} = 
		 extract_element(upgradePackageId, UpE),
	     MoRef = "ManagedElement=1,SystemFunctions=1,SwM=1,UpgradePackage="
		 ++Id,
	     Action = 
		  {'ManagedElement', 
		   [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
		   [{managedElementId, [], [MeId]},
		    {'SystemFunctions',
		     [{systemFunctionsId,[],["1"]},
		      {'SwM',
		       [],
		       [{swMId,[],["1"]},
			{removeUpgradePackage, 
			 [{upgradePackage, [MoRef]}]}]}]}]},

 
	     {ok, Response} = netconf(action, [nc1, Action]),
	     {ok, {returnValue, _,[ReturnValue]}} = 
		 extract_element(returnValue, Response),
	     ct:pal("removeUpgradePackage ~s~n~p~n",[MoRef,ReturnValue]),
	     IsBlocked = get_version(MoRef) == get_version(ActiveSwVersion),
	     case wait_for_progress(reportProgress, ActionId, ProgressFilter) of
		 ["SUCCESS"] ->
		     ok;
		 ["FAILURE"] when IsBlocked ->
		     ok;
		 ["FAILURE"] ->
		     {ok, Progress} = netconf(get, [nc1, ProgressFilter]),
		     {ok, {resultInfo, _, [ResultInfo]}} = 
			 extract_element(resultInfo, Progress),
		     case string:str(ResultInfo, "referred by a backup") of
			 0 ->
			     ct:pal("removeUpgradePackage failed:~n"),
			     ct:fail(["FAILURE"]);
			 _ ->
			     ok
		     end;
		 WfpResult ->
		     ct:pal("removeUpgradePackage failed: ~p~n",[WfpResult]),
		     ct:fail(WfpResult)
	     end;
	 _ ->
	     ok
     end||Element<-Content],
    ok.

    get_swm_content(MeId) ->
    Get = {'ManagedElement', 
	   [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId, [], [MeId]},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'SwM',
	       [],
	       [{swMId,[],["1"]}]}]}]},
    {ok, Result} = netconf(get, [nc1, Get]),
    {ok, {'SwM', _, Content}} = extract_element('SwM', Result),
    Content.

get_active_sw_version(MeId) ->
    Get = {'ManagedElement', 
	   [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId, [], [MeId]},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'SwInventory',
	       [],
	       [{swInventoryId,[],["1"]},
		{active,[],[]}
	       ]}]}]},
    {ok, Result} = netconf(get, [nc1, Get]),
    {ok, {active, _, [ActiveSwVersion]}}  =
	extract_element(active, Result),
    ActiveSwVersion.

get_version(MoRef) ->
    lists:last(string:tokens(MoRef, "=")).
    

%%%--------------------------------------------------------------------
%%% LIBRARY FUNCTIONS
%%%--------------------------------------------------------------------

get_ups(Config) ->
    MeId = proplists:get_value(meId, Config),
    Get_config = {'ManagedElement',
		  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		  [{managedElementId,[],[MeId]},
		   {'SystemFunctions',
		    [{systemFunctionsId,[],["1"]},
		     {'SwM',
		      [],
		      [{swMId,[],["1"]}]}]}]},
    case netconf(get_config, [nc1, running, Get_config]) of
	{ok, [{'ManagedElement',
	       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	       [{managedElementId,[],[MeId]},
		{'SystemFunctions',[],
		 [{systemFunctionsId,[],["1"]},
		  {'SwM',
		   _,
		   Packages}]}]}]} ->
	    [Label ||{'UpgradePackage',[],[{upgradePackageId,[],[Label]}|_]} <- Packages];
	{ok, Reply} ->
	    ct:pal("Reply = ~p~n",[Reply]),
	    ct:fail("Unexpected netconf Reply");
    	{error, Error} -> 
	    ct:pal("get_config Error = ~p~n",[Error]),
	    ct:fail("get_config Error")
    end.


up_action_generic(_Config, UpAction, Label, MeId) ->
    ct:pal("Execution action ~w",[UpAction]),
    Key = make_key(Label),
    ProgressFilter =  {'ManagedElement', 
		       [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
		       [{managedElementId, [MeId]},
			{'SystemFunctions',
			 [{systemFunctionsId,["1"]},
			  {'SwM',
			   [],
			   [{swMId,["1"]},
			    {'UpgradePackage',
			     [{upgradePackageId, [Key]},
			      {reportProgress, []}]}]}]}]},

    ActionId = get_action_id(ProgressFilter),
    ct:pal("Previous action id was ~p.",[ActionId]),
    Action =  {'ManagedElement', 
    	       [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
    	       [{managedElementId, [], [MeId]},
    		{'SystemFunctions',
    		 [{systemFunctionsId,[],["1"]},
    		  {'SwM',
    		   [],
    		   [{swMId,[],["1"]},
		    {'UpgradePackage', [], 
		     [{upgradePackageId, [Key]},
		      {UpAction, [], []}]}]}]}]},
    ct:pal("Calling action"),

    Start1 = os:timestamp(),

    case netconf(action, [nc1, Action]) of
    	{error, Error} -> 
	    ct:pal("ActionError = ~p~n",[Error]),
	    ct:fail("Action could not be started");
	{ok, A} ->
	    {ok, {returnValue, _, [ActionResult]}} = 
		extract_element(returnValue, A),
	    ct:pal("Action result: ~s",[ActionResult]), 
	    case ActionResult of
		"true" -> ok;
		"false" ->
		    ct:fail("Action could not start")
	    end
    end,

    ct:pal("Wait for progress"),
    case wait_for_progress(reportProgress, ActionId, ProgressFilter) of
    	["SUCCESS"] ->
    	    ok;
    	WfPResult ->
    	    ct:pal("~w: ~s",[UpAction, WfPResult]),
    	    ct:fail(WfPResult)
    end,
    
    End1 = os:timestamp(),

    Time = trunc(timer:now_diff(End1, Start1) / 1000/1000),
    Time.

get_action_id(ProgressFilter) ->
    {ok, A} = netconf(get, [nc1, ProgressFilter]),
    case extract_element(reportProgress, A) of
	{ok, {reportProgress, KeyList, []}} ->
	    case lists:member({unset, "true"}, KeyList) of
		true ->
		    undefined;
		false ->
		    {ok, {actionId, [], [ActionId]}} = extract_element(actionId, A),
		    ActionId
	    end;
	_ ->
	    {ok, {actionId, [], [ActionId]}} = extract_element(actionId, A),
	    ActionId
    end.

assert_release(_MeId, _Release, _Mode) ->
    ok.

%%%--------------------------------------------------------------------
%%% Description: Loop until the progress information says FINISHED
%%%              Requires a name and a netconf filter extracting the
%%%              progress report attribute
%%%--------------------------------------------------------------------
wait_for_progress(Attribute, OldActionId, ProgressFilter) ->
    case netconf_open(nc1, []) of
	{ok, _} ->
	    Result = 
		loop_wait_for_progress(Attribute, OldActionId, ProgressFilter),
	    ct_netconfc:close_session(nc1),
	    Result;
	{error, {connection_exists, _}} ->
	    timer:sleep(?Sleep),
	    ct_netconfc:close_session(nc1),
	    wait_for_progress(attribute, OldActionId, ProgressFilter);
	{error, Reason} ->
	    ct:log("~p~n",[Reason]),
	    timer:sleep(?Sleep),
	    wait_for_progress(Attribute, OldActionId, ProgressFilter)
    end.

wait_for_backup_progress(Attribute, OldActionId, ProgressFilter) ->
    timer:sleep(1000),
    case ct_netconfc:open(nc1, []) of
	{ok, _} ->
	    Get = ct_netconfc:get(nc1, ProgressFilter),
	    ct_netconfc:close_session(nc1),
	    case Get of
		{ok, Report} ->
		    case extract_element(actionId, Report) of
			{ok, {actionId, _, [OldActionId]}} ->
			    ct:pal("Waiting for updated progress ~s~n~p~n",
				   [OldActionId, Report]),
			    wait_for_backup_progress(Attribute, OldActionId,
					      ProgressFilter);
			_ ->
			    case check_backup_progress_elements(Attribute, Report) of
				loop ->
				    wait_for_backup_progress(Attribute, OldActionId,
						      ProgressFilter);
				{ok, Result} ->
				    Result
			    end
		    end;
		{error, Error} ->
		    ct:pal("Netconf get error:~n~p",[Error]),
		    wait_for_backup_progress(Attribute, OldActionId, ProgressFilter)
	    end;
	{error, Reason} ->
	    ct:log("Netconf open error:~n~p",[Reason]),
	    wait_for_backup_progress(Attribute, OldActionId, ProgressFilter)
    end.

%%%--------------------------------------------------------------------
%%% Description: Loop until the progress information says FINISHED
%%%              Requires a name and a netconf filter extracting the
%%%              progress report attribute
%%%--------------------------------------------------------------------
wait_for_esi_progress(Attribute, ProgressFilter) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    {ok, A} = ct_netconfc:get(nc1, ProgressFilter),
    ct_netconfc:close_session(nc1),
    {ok, Report} = extract_element(Attribute, A),
    {ok, State} = extract_element(state, [Report]),
    timer:sleep(1000),
    case State of
	{state, _, ["FINISHED"]} ->
	    ct:log("~p~n",[Report]),
	    {ok, {result, _, Result}} = extract_element(result, [Report]),
	    {ok, {resultInfo, _, ResultInfo}} = 
		extract_element(resultInfo, [Report]),
	    {Result, ResultInfo};
	{state, _, ["CANCELLED"]} ->
	    {ok, {result, _, Result}} = extract_element(result, [Report]),
	    {ok, {resultInfo, _, ResultInfo}} = 
		extract_element(resultInfo, [Report]),
	    {Result, ResultInfo};
	{state, _, [Current]} ->
	    ct:log("State: ~s~n",[Current]),
	    wait_for_esi_progress(Attribute, ProgressFilter)
    end.


loop_wait_for_progress(Attribute, OldActionId, ProgressFilter) ->
    timer:sleep(?Sleep),
    Get = ct_netconfc:get(nc1, ProgressFilter), 
    case Get of
	{ok, Report} ->
	    case extract_element(actionId, Report) of
		{ok, {actionId, _, [OldActionId]}} ->
		    ct:pal("Waiting for updated progress~n"),
		    wait_for_progress(Attribute, OldActionId, 
				      ProgressFilter);
		not_found ->
		    wait_for_progress(Attribute, OldActionId,
				      ProgressFilter);
		_ ->
		    case check_progress_elements(Attribute, Report) of
			loop ->
			    loop_wait_for_progress(Attribute, OldActionId, 
						   ProgressFilter);
			{ok, Result} ->
			    Result
		    end
	    end;
	{error, Error} ->
	    ct:log("~p",[Error]),
	    wait_for_progress(Attribute, OldActionId, ProgressFilter)
    end.

check_progress_elements(Attribute, ProgressReport) ->
    {ok, Report} = extract_element(Attribute, ProgressReport),
    {ok, State} = extract_element(state, [Report]),
    case State of
	{state, _, ["FINISHED"]} ->
	    format_progress_report(Report),
	    ct:pal("~s",[format_progress_report(Report)]),
	    {ok, {result, _, Result}} = extract_element(result, [Report]),
	    {ok, Result};
	{state, _, [Current]} ->
	    {ok, {progressPercentage, _, [Percent]}} =
		extract_element(progressPercentage, [Report]),
	    {ok, {progressInfo, _, [Info]}} = 
		extract_element(progressInfo, [Report]),
	    ct:pal("State: ~s ~s % ready~n~s",[Current, Percent, Info]),
	    loop
    end.

check_backup_progress_elements(Attribute, ProgressReport) ->
    {ok, Report} = extract_element(Attribute, ProgressReport),
    case Report of
	{progressReport, [{unset, "true"}], []} ->
	    loop;
	_ ->
	    {ok, State} = extract_element(state, [Report]),
	    case State of
		{state, _, ["FINISHED"]} ->
		    format_progress_report(Report),
		    ct:pal("~s",[format_progress_report(Report)]),
		    {ok, {result, _, Result}} =
			extract_element(result, [Report]),
		    {ok, Result};
		{state, _, [Current]} ->
		    {ok, {progressPercentage, _, [Percent]}} =
			extract_element(progressPercentage, [Report]),
		    {ok, {progressInfo, _, [Info]}} =
			extract_element(progressInfo, [Report]),
		    {ok, {actionName, _, [ActionName]}} =
			extract_element(actionName, [Report]),
		    ct:pal("Action: ~s~nState: ~s ~s % ready~n~s",
			   [ActionName, Current, Percent, Info]),
		    loop
	    end
    end.

format_progress_report({reportProgress, _, Members}) ->
    [io_lib:format("reportProgress:~n",[])|format_progress_report(Members)];
format_progress_report({progressReport, _, Members}) ->
    [io_lib:format("progressReport:~n",[])|format_progress_report(Members)];
format_progress_report([{Key, _, [Value]}|Members]) ->
    [io_lib:format("~w: ~s ~n",[Key, Value])|
     format_progress_report(Members)];
format_progress_report([{Key, _, []}|Members]) ->
    [io_lib:format("~w: ~s ~n",[Key, ""])|
     format_progress_report(Members)];
format_progress_report([]) -> [];
format_progress_report(X) -> 
    ct:pal("~p~n",[X]),
    ct:fail(unknown_progress_report_format).
	
		      
%%%--------------------------------------------------------------------
%%% Description: Netconf
%%%--------------------------------------------------------------------

netconf(F, A) ->
    swm_test_lib:check_nc_session(nc1),
    Res = apply(ct_netconfc, F, A),
    ok = ct_netconfc:close_session(nc1),
    Res.

%%%--------------------------------------------------------------------
%%% Description: Construct sftp
%%%--------------------------------------------------------------------

sftp_uri(Config) ->
    Host = proplists:get_value(sftp_host, Config),
    User = proplists:get_value(sftp_user, Config),
    Dir = proplists:get_value(sftp_root, Config),
    "sftp://"++User++"@"++Host++Dir.


%%%--------------------------------------------------------------------
%%% Description: From a general short xml syntax, extract a certain xml
%%%              element
%%%--------------------------------------------------------------------

extract_element(Element, [{Element, Attribute, Content}|_]) ->
    {ok, {Element, Attribute, Content}};
extract_element(Element, [{Element, Content}|_]) ->
    {ok, {Element, Content}};
extract_element(Element, [{_, _, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [{_, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [_|Elements]) ->
    extract_element(Element, Elements);
extract_element(_, []) ->
    not_found.

%%%--------------------------------------------------------------------
%%% Doc
%%%--------------------------------------------------------------------
unique_name() ->
    {A,B,C} =  os:timestamp(),
    lists:flatten(io_lib:format("~w-~6..0w-~6..0w",[A,B,C])).


%%%--------------------------------------------------------------------
%%% Description: Generate a mo key from a label
%%%--------------------------------------------------------------------

make_key([$_|T]) ->
    [$/|make_key(T)];
make_key([X|T]) ->
    [X|make_key(T)];
make_key([]) -> [].


%%%--------------------------------------------------------------------
%%% Description: Get latest up label
%%%--------------------------------------------------------------------
get_latest_up(Config) ->
    ct:log("Config:~n~p~n",[Config]),
    case ?config(saved_config, Config) of
	{_Saver, ConfigList} ->
	    ct:log("Saved ConfigList: ~p",[ConfigList]),
	    {uplabel, Label} = lists:keyfind(uplabel, 1, ConfigList),
	    Label;
	undefined ->
	    UpList = get_ups(Config),
	    ct:log("UpList: ~p",[UpList]),
	    [Label | _] = lists:reverse(UpList), % Latest UP
	    ct:log("UP does not exist in Config, try last UP from UP list" ,[]),
	    Label
    end,
    ct:pal("Label: ~p",[Label]),
    Label.


%%%--------------------------------------------------------------------
%%% Description: Get report progress
%%%--------------------------------------------------------------------
get_report_progress(Session, MeId, ProgressFilter) ->
    {ok,
     [{'ManagedElement',
       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
       [{managedElementId,[],[MeId]},
	{'SystemFunctions',[],
	 [{systemFunctionsId,[],["1"]},
	  {'SwM',
	   [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
	   AllAttr} ]}]}]} =
	netconf(get, [Session, ProgressFilter]),
    [AllReportProgress] = [X || {reportProgress, _ , _} = X <- AllAttr],
    {reportProgress, [{struct,_}] , ReportProgress} = AllReportProgress,
    ReportProgress.

%%%--------------------------------------------------------------------
%%% Description: Get latest UP
%%%--------------------------------------------------------------------
get_up_package(ReportProgress) ->
    ResultInfo = [X || {resultInfo, _ , _} = X <- ReportProgress],
    ct:pal("ResultInfo: ~p",[ResultInfo]),
    [{resultInfo,[],[UpgradePackage]}] = ResultInfo,
    ct:pal("UpgradePackage: ~p",[UpgradePackage]),
    
    UPLabel = lists:last(string:tokens(UpgradePackage,"=")),
    
    ct:pal("UPLabel: ~p",[UPLabel]),
    UPLabel.

%%### Functions for secure boards ######%%
%%% ===========================================================================
%%% @doc
%%% Description: Get Managed Element Id. Print out SwM.<br/>
%%% Session = atom() <br/>
%%% MeId = string()
%%% @spec get_me_id(Session) -> MeId
%%% @end
%%% ===========================================================================
get_me_id(Session) ->
    netconf_open(Session, []),
    {ok,
     [{'ManagedElement',
       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
       [{managedElementId,[],[MeId]}]}]} =
	ct_netconfc:get(Session,
			{'ManagedElement',
			 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			 [{managedElementId,[],[]}]
			}),
    ct_netconfc:close_session(Session),
    ct:pal("MeId:~n~p~n",[MeId]),
    MeId.

check_if_vc_board()->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
    Secure_board.
netconf_open(Session, Param)->
   
    case check_if_vc_board()  of
	"yes" -> ct_netconfc:open(Session, [{user, "SysAdminTest"},{password, "SysAdminTest"}|Param]);
	_ ->ct_netconfc:open(Session,Param)
    end.


%%% ===========================================================================
%%% @doc
%%% Description: Specific upgrade action will be done. <br/>
%%%              Match the expected result is the same as recieved.<br/>
%%% NC_Session = Netconf session <br/>
%%% ExpResult = string(), Expected result,  "SUCCESS" | FAILURE <br/>
%%% ActionName = string(), "create" | "remove" <br/>
%%% MeId = string() <br/>
%%% ActionId = string() , previous action id. <br/>
%%% @spec  wait_for_swm_progress_done(NC_Session, ExpResult, ActionName, MeId, ActionId) -> ok
%%% @end
%%% ===========================================================================
%%%%
%% Used when create and remove is used.
%%%%
wait_for_swm_progress_done(NC_Session, ExpResult, ActionName,
		       MeId, ActionId) ->
    wait_for_swm_progress_done(NC_Session, ExpResult, ActionName,
				  MeId, ActionId, dummy).

wait_for_swm_progress_done(NC_Session, ExpResult, ActionName, MeId,
		       ActionId, ErlNode) ->
    case wait_for_progress_result(NC_Session, MeId, ActionId, ErlNode,
				 ActionName) of
    	[{ExpResult, ActionName, ProgressInfo, ResultInfo,
	  State, _ProgReport}] ->
    	    ct:pal("result:~p~n"
    		   "actionName:~p~n"
    		   "progressInfo:~p~n"
    		   "resultInfo:~p~n"
		   "state:~p~n",[ExpResult,
				 ActionName,
				 ProgressInfo,
				 ResultInfo,
				 State]),
    	    ok;
    	Result ->
    	    ct:pal("Progress report not expected: ~p",[Result]),
    	    ct:fail(Result)
    end.

wait_for_progress_result(Session, MeId, OldActionId, Node, Action) ->
    [UP_Label | _] = lists:reverse(swm_test_lib:get_ups(Session)),
        %% {ok,_} = ct_netconfc:open(Session, [{timeout, 30000}]),
    netconf_open(Session, [{timeout, 30000}]),
    wait_for_progress_result(Session, MeId, OldActionId, Node, Action,
			     UP_Label, 3600000).

wait_for_progress_result(Session, _MeId, _OldActionId, _Node, _Action,
			 _UP_Label, Timeout)
  when Timeout < 0 ->
    ct_netconfc:close_session(Session, 30000),
    ct:fail("No Expected progress results within max timeout.");

wait_for_progress_result(Session, MeId, OldActionId, Node, Action,
			 UP_Label, Timeout) ->
    ct:pal("¤¤ UP_Label : ~p", [UP_Label]),
    ct:pal("¤¤ Action : ~p", [Action]),

    case Node of
	dummy ->
	    ok;
	_ ->
	    ok = check_nc_session(Session, Node)
    end,

    case Action of
	Action when Action == "createUpgradePackage";
		    Action == "removeUpgradePackage";
		    Action == "swm_progess_report"->
	    ct:pal("¤¤¤¤¤¤¤¤ check swm prog report ¤¤¤¤¤", []),
	    {reportProgress,
	     [{struct,"AsyncActionProgress"}],
	     ProgressReport} = get_report_progress_vc(Session, MeId),
	    ProgressReport;

	_Action ->
	    ProgressReport =  get_up_report_progress(Session, MeId, UP_Label),

	    ok

    end,

    {actionId,_,[ActionId]} = lists:keyfind(actionId, 1, ProgressReport),
    case ActionId of
	OldActionId ->
	    ct:pal("Waiting for updated progress~n: old actionId : ~p ",
		   [OldActionId]),
	    timer:sleep(1000),
	    wait_for_progress_result(Session, MeId, OldActionId, Node,
				     Action, UP_Label, Timeout-1000);
	_ ->
	    case check_progress_elements_vc(ProgressReport) of
		loop ->
		    timer:sleep(1000),
		    wait_for_progress_result(Session, MeId, OldActionId, Node,
					     Action, UP_Label, Timeout-1000);
		{ok, Result} ->
		    ct:log("Result: ~p", [Result]),
		    ct_netconfc:close_session(Session, 30000),
		    ct:pal("New ActionId: ~p, OldActionId: ~p", [ActionId,
								 OldActionId]),
		    Result
	    end
    end.


%%% ===========================================================================
%%% @doc
%%% Description: check nc session exist, if not then check node is up.<br/>
%%%              If not then wait for node and nc sessions is up.<br/>
%%% Session = atom() <br/>
%%% Node = ErlNode, dummy if Node is not used. <br/>
%%% @spec check_nc_session(Session) -> ok
%%% @end
%%% ===========================================================================
check_nc_session(Session) ->
    check_nc_session(Session, dummy).
check_nc_session(Session, Node) ->
    check_nc_session(Session, Node, 600000). %% 10 min

check_nc_session(_Session, _Node, Timeout) when Timeout < 0 ->
    ct:fail("NC session not up within max timeout.");

check_nc_session(Session, Node, Timeout) ->
    case netconf_open(Session, [{timeout, 30000}]) of
	{ok,_} ->
	    ok;
	{error,{connection_exists, _}} ->
	    ok;
	_Err ->
	    ct:log("¤¤¤ nc_session not open: ~p. Sleep and try again",[_Err]),
	    timer:sleep(?Sleep),
	    check_nc_session(Session, Node, Timeout-?Sleep)
    end.
%%%--------------------------------------------------------------------
%%% Description: get_report_progress_vc
%%%--------------------------------------------------------------------
get_report_progress_vc(Session, MeId) ->
    get_report_progress_vc(Session, MeId, 60000).
get_report_progress_vc(_Session, _MeId, Timeout) when Timeout < 0 ->
    ct:fail("No progress report rceived within max timeout.");
get_report_progress_vc(Session, MeId, Timeout) ->
    case ct_netconfc:get(Session,
			 {'ManagedElement',
			  [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
			  [{managedElementId, [], [MeId]},
			   {'SystemFunctions',
			    [{systemFunctionsId,[],["1"]},
			     {'SwM',
			      [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
			      [{swMId,[],["1"]},
			       {reportProgress, []}
			      ]}]}]}, 30000) of
	{ok,
	 [{'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],["1"]},
	    {'SystemFunctions',[],
	     [{systemFunctionsId,[],["1"]},
	      {'SwM',
	       [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
	       AllAttr} ]}]}]} ->
	    %% Filter out reportProgress
	    [ReportProgress] = [X || {reportProgress, _ , _} = X <- AllAttr],
	    %% ct:pal("ReportProgr: ~p ",[ReportProgress]),
	    ReportProgress;
	_Err ->
	    ct:log("No progress report recived: ~p ~n"
		   "Check nc session. ~n",[_Err]),
	    timer:sleep(1000),
	    check_nc_session(Session),
	    get_report_progress(Session, MeId, Timeout-1000)
    end.

%%% ===========================================================================
%%% @doc
%%% Description: Get report progress from specific UpgradePackage. <br/>
%%% Nc = Netconf session <br/>
%%% MeId = string() <br/>
%%% Label = string() , ex "CXS101549/3-R2A2956" <br/>
%%% @spec get_up_report_progress(NC_Session, MeId, Label) -> ReportProgress
%%% @end
%%% ===========================================================================
get_up_report_progress(NC_Session, MeId, UP_Label) ->
    get_up_report_progress(NC_Session, MeId, UP_Label, 60000).

get_up_report_progress(_NC_Session, _MeId, _UP_Label, Timeout)
  when Timeout < 0 ->
    ct:fail("No up progress report rceived within max timeout.");

get_up_report_progress(NC_Session, MeId, UP_Label, Timeout) ->
    ct:pal("### UP_Label: ~p", [UP_Label]),
    case ct_netconfc:get(NC_Session,
			 {'ManagedElement',
		   [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
		   [{managedElementId, [], [MeId]},
		    {'SystemFunctions',
		     [{systemFunctionsId,[],["1"]},
		      {'SwM',
		       [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
		       [{swMId,[],["1"]},
			{'UpgradePackage', [],
			 [{upgradePackageId,[],[UP_Label] }] }
		       ]}]}]}, 30000) of

	{ok, [{'ManagedElement',
	       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	       [{managedElementId,[],["1"]},
		{'SystemFunctions',[],
		 [{systemFunctionsId,[],["1"]},
		  {'SwM',
		   [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
		   [{swMId,[],["1"]},
		    {'UpgradePackage',[], UPInfoList}
		   ]}]}]}] } ->

	    ct:log("UPInfoList:~p",[UPInfoList]),
	    case lists:keyfind(reportProgress, 1, UPInfoList) of
		{reportProgress,[_], ReportProgress } ->
		    ct:pal("### UP_ReportProgress: ~p", [ReportProgress]),
		    ReportProgress;
		_Other ->
		    ct:log("Other:~p",[_Other]),
		    timer:sleep(?Sleep),
		    get_up_report_progress(NC_Session,
					   MeId,
					   UP_Label,
					   Timeout-?Sleep)
	    end;
	_Err ->
	    ct:log("No progress report recived: ~p ~n"
		   "Check nc session. ~n",[_Err]),
	    timer:sleep(1000),
	    check_nc_session(NC_Session),
	    get_up_report_progress(NC_Session, MeId, UP_Label, Timeout-1000)
    end.
%%%--------------------------------------------------------------------
%%% Description: wait for FINISHED in  progress results and return results.
%%%--------------------------------------------------------------------
check_progress_elements_vc(ProgressReport) ->
    check_progress_elements_vc(ProgressReport, "FINISHED").

check_progress_elements_vc(ProgressReport, ExpState) ->
    {state,[],[State]} =
	lists:keyfind(state, 1, ProgressReport),
    %% ct:pal("### ~p",[State]),
    ct:pal("# ~p, ~p",[State, ExpState]),
    case State of
	 ExpState->
	    ct:log("# ~p~n",[ProgressReport]),
	    {result,[],[Result]} =
		lists:keyfind(result, 1, ProgressReport),
	    {actionName,[],[ActionName]} =
		lists:keyfind(actionName, 1, ProgressReport),
	    {progressInfo,[],[ProgressInfo]} =
		lists:keyfind(progressInfo, 1, ProgressReport),
	    case lists:keyfind(resultInfo, 1, ProgressReport) of
		{resultInfo,[],[ResultInfo]} ->
		    ResultInfo;
		{resultInfo,[],[]} ->
		    ResultInfo = []
	    end,
	    {state,[],[State]} =
    		lists:keyfind(state, 1, ProgressReport),
	    {ok, [{Result, ActionName, ProgressInfo, ResultInfo, State,
		  ProgressReport}]};
	CurrentState -> %% Ej klar
	    {progressPercentage,[],[Percent]} =
		lists:keyfind(progressPercentage, 1, ProgressReport),
	    {progressInfo,[],[Info]} =
		lists:keyfind(progressInfo, 1, ProgressReport),
		ct:pal("# State: ~s ~p % ready~n~s",[CurrentState,
						     list_to_integer(Percent),
						     Info]),
	    loop
    end.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
perform_ug_action(Action, _Config, Label, MeId) ->
    Start1 = os:timestamp(),
    ct:pal("Perform ug action:~n~p~n",[Action]),
    swm_test_lib:ug_action_match_result(?NC_Session,
    					"SUCCESS",
    					Label,
    					MeId,
    					Action),

    End1 = os:timestamp(),

    Time = trunc(timer:now_diff(End1, Start1) / 1000/1000),
    Time.


%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
create_backup(Config)->
    MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config),

    ProgressFilter =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'BrM',
	     [{brMId,[],["1"]},
	      {'BrmBackupManager',
	       [{brmBackupManagerId,[],["1"]},
		{progressReport, []}]}]}]}]},
    ActionId = get_action_id(ProgressFilter),
    Action = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],[MeId]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'BrM',
		  [{brMId,[],["1"]},
		   {'BrmBackupManager',
		    [{brmBackupManagerId,[],["1"]},
		     {'createBackup',[],
		      [{name, [], [Name]}]}]}]}]}]},
    ct:pal("Executing action createBackup"),
    case netconf(action, [nc1, Action]) of
	{ok, ActionResponse} ->
	    case extract_element(returnValue, ActionResponse) of
		{ok, {returnValue, _, ["0"]}} ->
		    ok;
		{ok, {returnValue, _, [ReturnValue]}} ->
		    Error = decode_return_value(ReturnValue),
		    ct:pal("Return value is ~w (~s)~n",[Error, ReturnValue]),
		    ct:fail(Error)
	    end;
	{error, ErrorResponse} ->
	    {ok, {'error-message', _, [ErrorMessage]}} =
		extract_element('error-message', ErrorResponse),
	    Error = decode_error_message(ErrorMessage),
	    ct:print("Action failure~n",[]),
	    ct:log("~s~n",[ErrorMessage]),
	    ct:fail(Error)
    end,
    case wait_for_backup_progress(progressReport, ActionId, ProgressFilter) of
	["SUCCESS"] ->
	    ct:pal("Backup succesfully created. Verifying metadata."),
	    %% DataDir = proplists:get_value(data_dir, Config), %% This dir is obsolete
	    DataDir = ?DataDir,
	    ct:pal("DataDir: ~p",[DataDir]),
	    SchemaFile = filename:join(DataDir, "backupinfo.xsd"),
	    ResultInfo = get_result_info(ProgressFilter),
	    ct:pal("ResultInfo ~p~n",[ResultInfo]),
	    BuId = lists:last(string:tokens(ResultInfo, "=")),
	    BuPath = rct_rpc:call(rpc_1, swmLib, backup_dir, [BuId],10000),
	    BuFile = filename:join(BuPath, "backupinfo.xml"),
	    {ok, Bin} = rct_rpc:call(rpc_1, file, read_file, [BuFile],10000),
	    PrivDir = proplists:get_value(priv_dir, Config),
	    MetadataFile = filename:join(PrivDir, "backupinfo.xml"),
	    file:write_file(MetadataFile, Bin),
	    Res = cmd(["xmllint --schema ", SchemaFile, " ", MetadataFile, " ; echo -n \"Res=$?\""]),
	    case lists:last(string:tokens(Res, "\n=")) of
		"0" ->
		    {ok, ResultInfo, ActionId};
		Other ->
		    Msg = xmllint_status(Other),
		    ct:pal("xmllint exited with status ~s ~s~n",[Other, Msg]),
		    ct:fail({xmllint, Msg})
	    end;
	Result ->
	    ct:pal("createBackup: ~s~n",[Result]),
	    ct:fail(Result)
    end.

export_backup(Config) ->
    MeId = proplists:get_value(meId, Config),
    %% Backups = get_all_backups(MeId),

    Name = "swm_netem_SUITE.export_backup."++unique_name(),
    NewConfig =
	lists:keyreplace(backupName, 1, Config,
			 {backupName, Name}),
    {ok, ResultInfo, ActionId} = create_backup(NewConfig),
    Index = [lists:last(string:tokens(ResultInfo, "="))],

    ct:pal("Selected backup ~p for export.~n",[Index]),
    ProgressFilter =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'BrM',
	     [{brMId,[],["1"]},
	      {'BrmBackupManager',
	       [{brmBackupManagerId,[],["1"]},
		{'BrmBackup',
		 [{brmBackupId, [], Index},
		  {progressReport, []}]}]}]}]}]},


    %% ActionId = get_action_id(ProgressFilter),
    ct:pal("ActionId = ~p~n",[ActionId]),

    Uri = sftp_uri(Config),
    Password = proplists:get_value(sftp_pass, Config),
    Action = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],[MeId]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'BrM',
		  [{brMId,[],["1"]},
		   {'BrmBackupManager',
		    [{brmBackupManagerId,[],["1"]},
		     {'BrmBackup',
		      [{brmBackupId, Index},
		       {'export',[],
			[{uri, [], [Uri]},
			 {password, [Password]}]}]}]}]}]}]},
    RootDir = proplists:get_value(sftp_root, Config),
    %% check_server_conditions(Uri, Password),
    Ares = netconf(action, [nc1, Action]),
    case Ares of
	{ok, _} -> ok;
	Error ->
	    ct:pal("backupExport action failed: ~p~n",[Error]),
	    ct:fail(Error)
    end,
    case wait_for_backup_progress(progressReport, ActionId, ProgressFilter) of
	["SUCCESS"] ->
	    {ok, Files} = file:list_dir(RootDir),
	    ct:pal("RootDir = ~p ~nFiles = ~p~n",[RootDir, Files]),
	    ok;
	Result ->
	    ct:pal("exportBackup: ~s~n",[Result]),
	    ct:pal("URI: ~s~n",[Uri]),
	    cmd(["ls -la ",RootDir]),
	    ct:fail(Result)
    end.

%%--------------------------------------------------------------------
%% @doc Removes all backups in the system
%% Using netconf to trig all backups to be removed.
%% @end
clean_backups(Config)->
    MeId = proplists:get_value(meId, Config),
    Backups = get_all_backups(MeId),

    [begin
	 {ok, {_, _, [Name]}} = extract_element(backupName, [Backup]),
	 case extract_element(creationType, [Backup]) of
	     {ok, {_, _, ["SYSTEM_CREATED"]}} ->
		 ok;
	     _ ->
		 NewConfig = lists:keyreplace(backupName,1, Config,
					      {backupName, Name}),
		 delete_backup([{force_backup_name, true}|NewConfig])
	 end
     end||Backup<-Backups],
    ok.

%%%--------------------------------------------------------------------
%%% Description: Returns a list of all backup elements
%%%--------------------------------------------------------------------

get_all_backups(MeId) ->
    Get =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'BrM',
	     [{brMId,[],["1"]},
	      {'BrmBackupManager',
	       [{brmBackupManagerId,[],["1"]}]}]}]}]},
    {ok, Result} = netconf(get, [nc1, Get]),
    {ok, {_, _, Contents}} = extract_element('BrmBackupManager', Result),
    [BrmBackupE||BrmBackupE<-Contents,
		 element(1, BrmBackupE) == 'BrmBackup'].

%%--------------------------------------------------------------------
%% Using netconf to trig a backup to be removed
%% Initiate the action and wait for it to be deleted.
%% @end

delete_backup(Config) ->
    MeId = proplists:get_value(meId, Config),
    %% Backups = get_all_backups(MeId),
    Name =
	case proplists:get_value(force_backup_name, Config, false) of
	    true ->
		proplists:get_value(backupName, Config);
	    false ->
		BName = "swm_netem_SUITE.delete_backup."++unique_name(),
		BName
	end,
    ct:pal("Selected backup ~p for deletion.~n",[Name]),

    ProgressFilter =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'BrM',
	     [{brMId,[],["1"]},
	      {'BrmBackupManager',
	       [{brmBackupManagerId,[],["1"]},
		{progressReport, []}]}]}]}]},
    %% ActionId = get_action_id(ProgressFilter),
    ActionId = 1, %% ActionId have no effect in this place

    Action = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],[MeId]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'BrM',
		  [{brMId,[],["1"]},
		   {'BrmBackupManager',
		    [{brmBackupManagerId,[],["1"]},
		     {'deleteBackup',[],
		      [{name, [], [Name]}]}]}]}]}]},
    ct:pal("Executing action deleteBackup ~s~n",[Name]),
    case netconf(action, [nc1, Action]) of
	{ok, ActionResponse} ->
	    case extract_element(returnValue, ActionResponse) of
		{ok, {returnValue, _, ["0"]}} ->
		    ok;
		{ok, {returnValue, _, [ReturnValue]}} ->
		    Error = decode_return_value(ReturnValue),
		    ct:pal("Return value is ~w (~s)~n",[Error, ReturnValue]),
		    ct:fail(Error)
	    end;
	{error, ErrorResponse} ->
	    {ok, {'error-message', _, [ErrorMessage]}} =
		extract_element('error-message', ErrorResponse),
	    Error = decode_error_message(ErrorMessage),
	    ct:print("Action failure~n",[]),
	    ct:log("~s~n",[ErrorMessage]),
	    ct:fail(Error)
    end,
    case wait_for_backup_progress(progressReport, ActionId, ProgressFilter) of
	["SUCCESS"] ->
	    ct:pal("Backup deleted successfully"),
	    ok;
	Result ->
	    ct:pal("deleteBackup: ~s~n",[Result]),
	    ct:fail(Result)
    end.

export_esi(Config)->
    EsiLogPath = ?config(priv_dir, Config),
    ct:pal("#### EsiLogPath export_esi ~p~n", [EsiLogPath]),
    [{host, SftpHost},{username, Username},{password, Password}] = ct:get_config(sftp_server),
    SFTP_URL = "sftp://"++Username++"@"++SftpHost,
    os:cmd("chmod 777 " ++ EsiLogPath), % else permission.
    Action = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],["1"]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'LogM',
		  [{xmlns,"urn:com:ericsson:ecim:LogM"}],
		  [{logMId,[],["1"]},
		   {'exportEsi',[],
		    [{uri, [], [SFTP_URL++EsiLogPath]},
		     {password, [], [Password]}]}
		    %% [{uri, [], [?SFTP_URL++EsiLogPath]},
		    %%  {password, [], [?PSWD]}]}
		  ]}]}]},
    ProgressFilter = {'ManagedElement',          
		      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		      [{managedElementId,[],["1"]},
		       {'SystemFunctions',
			[{systemFunctionsId,[],["1"]},
			 {'LogM',
			  [{xmlns,"urn:com:ericsson:ecim:LogM"}],
			  [{logMId,[],["1"]},
			   {progressReport,[],[]}
			  ]}]}]},
    {ok,_} = ct_netconfc:open(nc1,[]),
    %% trig the action export_esi using netconf.
    case ct_netconfc:action(nc1, Action) of
	{ok, A} ->
	    Return = extract_element(returnValue, A),
	    ct:pal("export_esi ~p~n",[Return]);
	Error ->
	    ct:pal("~p~n",[Error]),
	    ct:fail("action error")
    end,
    ct_netconfc:close_session(nc1),
    timer:sleep(500),
    te(wait_for_esi_progress(progressReport, ProgressFilter), EsiLogPath).

te({["SUCCESS"], [EsiLogName]}, EsiLogPath) ->
    ct:pal("export_esi: SUCCESS~n",[]),
    check_esi_log_transfered(EsiLogName, EsiLogPath);
te(Result, _) ->
    ct:pal("export_esi: ~p~n",[Result]),
    ct:fail(Result).

%% @hidden
%% Will check that esi log is tranfered to expected path and also the size of the file.
%% If the size is to small, then maybe it does not consist on anything!
check_esi_log_transfered(EsiLogName, EsiLogPath) ->
    ct:pal("#### EsiLogPath check_esi_log_transfered ~p~n", [EsiLogPath]),
    [{host, SftpHost},{username, Username},{password, Password}] = ct:get_config(sftp_server),
    {ok,ChPid,_ChRef} = 
	ssh_sftp:start_channel(SftpHost, 
			       [{user, Username},
				{password, Password},
				{silently_accept_hosts, true},
				{timeout, 10000}]),
    {ok, DirData} = ssh_sftp:list_dir(ChPid, EsiLogPath, 2000),
    % DirData = lists of strings from the directory.
    Pred= fun(Str) ->
		  if Str == EsiLogName ->
			  true;
		     true ->
			  false
		  end
	  end,

    case lists:any(Pred,DirData) of
	true -> 
	    ct:pal("### Esi log file: ~p, exist in \n path : ~p \n",
		   [EsiLogName, EsiLogPath]);
	false -> 
	    ct:fail(" Could not find the ESI log file, on sftp server.")
    end,
	     
    {ok, FileInfo} = 
	ssh_sftp:read_file_info(ChPid, EsiLogPath++EsiLogName, 2000),
    ct:pal("### Recieved FileInfo: ~p", [FileInfo]),

    Size = lists:nth(2, tuple_to_list(FileInfo)),
    %{file_info, Size, _, _, _, _, _, _, _, _, _, _, _, _} = FileInfo,

    if Size > 10000 ->
	    ct:pal("### Recieved Size: ~p", [Size]),
	    true;
       true  ->
	    ct:pal("### Size of the esi tar file is: ~p. ~n "
		   "It is smaller than expected. ~n "
		   "Unpack the file and ckeck that it look OK. \n",[Size]),
	    ct:fail("Size of the esi log file is to small! check if it "
		    "looks ok after unpack!.")
    end,  

    X = ssh_sftp:stop_channel(ChPid),
    ct:pal("### stop sftp channel ~p", [X]),
 
    {EsiLogPath, EsiLogName}.



%%%--------------------------------------------------------------------
%%% Description: Translate xmllint exit codes to strings
%%%--------------------------------------------------------------------

xmllint_status("1") -> "Unclassified";
xmllint_status("2") -> "Error in DTD";
xmllint_status("3") -> "Validation error";
xmllint_status("4") -> "Validation error";
xmllint_status("5") -> "Error in schema compilation";
xmllint_status("6") -> "Error writing output";
xmllint_status("7") -> "Error in pattern";
xmllint_status("8") -> "Error in Reader registration";
xmllint_status("9") -> "Out of memory error".

%%%--------------------------------------------------------------------
%%% Description: Convert an backup action error code to a meaningful value
%%%--------------------------------------------------------------------

decode_return_value("0") -> actionStarted;
decode_return_value("1") -> nameValidationFailure;
decode_return_value("2") -> duplicateName;
decode_return_value("3") -> housekeepingRequired;
decode_return_value("4") -> backupNotFound;
decode_return_value("98") -> missingParameter;
decode_return_value("99") -> softwareFault.

%%%--------------------------------------------------------------------
%%% Description: Convert an backup action error message to a matchable atom
%%%--------------------------------------------------------------------

decode_error_message(Msg) ->
    Codes = 
	[{"Name validation failure", nameValidationFailure},
	 {"Duplicate name", duplicateName},
	 {"Housekeeping required", housekeepingRequired},
	 {"Backup not found", backupNotFound},
	 {"Function busy", functionBusy},
	 {"Missing parameter", missingParameter},
	 {"Software fault", softwareFault}],
    [Result] = 
	[Code||{Pattern, Code}<-Codes,
	       case re:run(Msg, Pattern) of
		   {match, _} ->
		       true;
		   _ ->
		       false
	       end],
    Result.

%%%--------------------------------------------------------------------
%%% Description: Evaluate an os cmd and print both the command and result
%%%--------------------------------------------------------------------

cmd(Cmd) ->
    ct:pal("~s~n~s~n",[lists:flatten(Cmd), R = os:cmd(Cmd)]),
    R.

%%%--------------------------------------------------------------------
%%% Description: Read the resultinfo info of the last progress report
%%%--------------------------------------------------------------------

get_result_info(ProgressFilter) ->
    get_progress_report_member(resultInfo, ProgressFilter).

%%%--------------------------------------------------------------------
%%% Description: Read a member of the progress report struct
%%%--------------------------------------------------------------------

get_progress_report_member(Member, ProgressFilter) ->
    {ok, A} = netconf(get, [nc1, ProgressFilter]),
    case extract_element(progressReport, A) of
	{ok, {progressReport, [{unset, "true"}], []}}  ->
	    undefined;
	_ ->
	    {ok, {Member, [], [Value]}} =
		extract_element(Member, A),
	    Value
    end.


%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_upg_crl_lib.erl %
%%% @author eransbn
%%% @copyright Ericsson AB 2017
%%% @version /main/R8A/R10A/1
-module(swm_upg_crl_lib).
-author('eransbn').
%%% ----------------------------------------------------------
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
%%% R8A/1      2017-01-31 eransbn     Created
%%% ---------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-export([create/3, prepare/2, verify/2, activate/2, confirm/2,
	 clean_up/1, run_upgrade/3]).


-define(Sleep, 1000).
%%%--------------------------------------------------------------------
%%% @doc Create an upgrade package, using the latest package found in clearcase
%%% @end
%%%--------------------------------------------------------------------
run_upgrade(InitConfig, NC_Session, UgPath) ->
    Newonfig = create(InitConfig, NC_Session,UgPath),
    Config = InitConfig ++ Newonfig,
    prepare(Config, NC_Session),   
    verify(Config, NC_Session),
    activate(Config, NC_Session),
    confirm(Config, NC_Session).

create(Config, NC_Session, UPPath) ->
    ct:log("####################  Create #################################"),

    From_Label = swm_test_lib:get_sw_version(NC_Session),
    ct:log("From UP: ~p",[From_Label ]),

    [{host, Host},{username, User},{password, Password}] = ct:get_config(sftp_server),
    Hw =atom_to_list(ct:get_config({test_nodes,1})),
    MeId = get_me_id(NC_Session),
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:log("UGPath: ~p",[UGPath]),
    Uri = "sftp://"++User++"@"++Host++UGPath,
    ct:log("URI ~p",[Uri]),
   
   
    [Up] = string:tokens(os:cmd("ls " ++ UPPath ),"\n"),
    UpgradePackage = os:cmd("/afs/sunrise.ericsson.se/se/env/RCSDE/bin/upgradeprep.sh -stp "++ Hw ++ " "  ++ filename:join(UPPath, Up)),
    ct:log("Printout from upgradeprep.sh: ~n~p",[UpgradePackage]),

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
	"yes" -> wait_for_swm_progress_done(NC_Session,
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
    ct:log("up package ~p",[Label]),

    Release = proplists:get_value(release, Config),
    ct:log("Release  ~p",[Release]),


    assert_release(MeId, Release, keep),


    %% Add created up label to Config. Add upgrade times.
    %% Will be used in other upgrade actions.


    NewConfig = [{from_label, From_Label},
		 {uplabel, Label},
		 {create_time, UgCreateTime}], 
    ct:log("NewConfig ~p",[NewConfig]),

    NewConfig.

%%%--------------------------------------------------------------------
%%% @doc Prepares an upgrade package, which means downloading a complete UP
%%% @end
%%%--------------------------------------------------------------------

prepare(Config, NC_Session) ->
ct:pal("Config ~p",[Config]),
    ct:log("####################  Prepare #################################"),
    Label = get_latest_up(Config, NC_Session),
  %% MeId = proplists:get_value(meId, Config),
MeId = get_me_id(NC_Session),
    UgPrepareTime = case check_if_vc_board() of
			"yes" ->
			    perform_ug_action(prepare, Config, Label, MeId, NC_Session);
			_ ->
			   up_action_generic(Config, prepare, Label, MeId, NC_Session)
		    end,
    ct:pal("UgPrepareTime: ~p~n",[UgPrepareTime]),

    Release = proplists:get_value(release, Config),
    assert_release(MeId, Release, keep),

    %% Add created up label to Config. Add upgrade times.
    %% Will be used in other upgrade actions.
    %% NewConfig = [{uplabel, Label}],
    %% {_Saver, ConfigList} = ?config(saved_config, Config),
    %% NewConfig = ConfigList ++ [{prepare_time, UgPrepareTime}],
    %% ct:pal("Add NewConfig: ~p",[NewConfig]),
    %% {save_config, NewConfig},
ok.

%%%--------------------------------------------------------------------
%%% @doc Verifies an upgrade package
%%% @end
%%%--------------------------------------------------------------------

verify(Config, NC_Session) ->
ct:log("####################  Verify #################################"),

    Label = get_latest_up(Config, NC_Session),
    MeId = proplists:get_value(meId, Config),

    UgVerifyTime = case check_if_vc_board() of
		       "yes" ->
			   perform_ug_action(verify, Config, Label, MeId, NC_Session );
		       _->
			   up_action_generic(Config, verify, Label, MeId, NC_Session)
		   end,
    ct:pal("UgVerifyTime: ~p~n",[UgVerifyTime]),

    Release = proplists:get_value(release, Config),
    assert_release(MeId, Release, keep),

    %% Add created up label to Config. Add upgrade times.
    %% Will be used in other upgrade actions.
    %% {_Saver, ConfigList} = ?config(saved_config, Config),
    %% NewConfig = ConfigList ++ [{verify_time, UgVerifyTime}],
    %% ct:pal("Add NewConfig: ~p",[NewConfig]),
    %% {save_config, NewConfig}.
ok.

%%%--------------------------------------------------------------------
%%% @doc Activates an upgrade package
%%% @end
%%%--------------------------------------------------------------------

activate(Config, NC_Session) ->
    ct:log("####################  Activate #################################"),
    Label = get_latest_up(Config, NC_Session),
    MeId = proplists:get_value(meId, Config),

    UgAcivateTime = case check_if_vc_board() of
			"yes" ->
			    perform_ug_action(activate, Config, Label, MeId, NC_Session);
			_->
			    up_action_generic(Config, activate, Label, MeId, NC_Session)
		    end,
    ct:pal("UgAcivateTime: ~p~n",[UgAcivateTime]),
    timer:sleep(20000),

    Release = proplists:get_value(release, Config),
    assert_release(MeId, Release, switch),

    %% Add created up label to Config. Add upgrade times.
    %% Will be used in other upgrade actions.
    %% {_Saver, ConfigList} = ?config(saved_config, Config),
    %% NewConfig = ConfigList ++ [{activate_time, UgAcivateTime}],
    %% ct:pal("Add NewConfig: ~p",[NewConfig]),
    %% {save_config, NewConfig}.
ok.

%% %%%--------------------------------------------------------------------
%% %%% @doc Commits an upgrade package (equivalent to confirm)
%% %%% @end
%% %%%--------------------------------------------------------------------

%% commit(Config) ->
%%     %% [Label | _] = lists:reverse(get_ups(Config)), % Latest UP
%%     Label = get_latest_up(Config),
%%     up_action_generic(Config, commit, Label),


%%     %% Add created up label to Config.
%%     %% Will be used in other upgrade actions.
%%     NewConfig = [{uplabel, Label}],
%%     ct:pal("Add NewConfig: ~p",[NewConfig]),
%%     {save_config, NewConfig}.

%%%--------------------------------------------------------------------
%%% @doc Confirms an upgrade package
%%% @end
%%%--------------------------------------------------------------------

confirm(Config, NC_Session) ->
    ct:log("####################  Confirm #################################"),
    Label = get_latest_up(Config, NC_Session),
    MeId = proplists:get_value(meId, Config),
    Key = make_key(Label),
    Start1 = os:timestamp(),
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
		      {confirm, [], []}]}]}]}]},
    ct:pal("Calling action"),
    case netconf(action, [NC_Session, Action]) of
    	{error, Error} ->
	    ct:pal("ActionError = ~p~n",[Error]),
	    ct:fail("Action confirm failed");
	{ok, A} ->
	    {ok, {returnValue, _, [ActionResult]}} =
		extract_element(returnValue, A),
	    ct:pal("Action result: ~s",[ActionResult]),
	    case ActionResult of
		"true" -> ok;
		"false" ->
		    ct:fail("Action confirm could not start")
	    end
    end,

    End1 = os:timestamp(),
    UgConfirmTime = trunc(timer:now_diff(End1, Start1) / 1000/1000),
    ct:pal("UgConfirmTime: ~p~n",[UgConfirmTime]),

    Release = proplists:get_value(release, Config),
    assert_release(MeId, Release, switch),
    ActiveSwVersion = get_active_sw_version(MeId),
    ct:log("Active SW ~p",[ActiveSwVersion]),
    %% Add created up label to Config. Add upgrade times.
    %% Will be used in other upgrade actions.
    %% {_Saver, ConfigList} = ?config(saved_config, Config),
    %% NewConfig = ConfigList ++ [{confirm_time, UgConfirmTime}],
    %% ct:pal("NewConfig: ~p",[NewConfig]),
    %% {save_config, NewConfig}.
    ok.


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


%apa
%%%--------------------------------------------------------------------
%%% LIBRARY FUNCTIONS
%%%--------------------------------------------------------------------
%%%--------------------------------------------------------------------
%%% Description: Test the OAM connectivity
%%% Name =           atom()       Only suport coli (can be built out)
%%% Timer =          integer()    Time between attempts.
%%% NumberAttempts = integer()    Number of Attempts.
%%% ExpectedResult =              Expected return.
%%%--------------------------------------------------------------------

%% poll_connection(HookName, _Timer, _Result, 0)->
%%     ct:fail("Tried too many times to connect to rct_~p", [HookName]) ;
%% poll_connection(HookName, Timer, Result, NumTry)->
%%     timer:sleep(Timer),
%%     case HookName of
%%         coli -> case rct_coli:connect(coli) of
%%                     Result -> ct:log("OK: ~p",[Result]),
%%                               rct_coli:disconnect(HookName),
%%                               ok;
%%                     _-> ok = rct_coli:disconnect(HookName),
%%                         poll_connection(HookName, Timer, Result, NumTry - 1)
%%                 end;
%%         _-> ct:fail("HookName ~p not suported",[HookName])
%%     end.

%%%--------------------------------------------------------------------
%%% Description: Returns a list of all UP labels.
%%% Example: ["CXS101549/1-R2A1460","CXS101549/1-R2A1461","CXS101549/1-R2A1462"]
%%%--------------------------------------------------------------------

get_ups(_Config, NC_Session) ->
   %% MeId = proplists:get_value(meId, Config),
MeId = get_me_id(NC_Session),
    Get_config = {'ManagedElement',
		  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		  [{managedElementId,[],[MeId]},
		   {'SystemFunctions',
		    [{systemFunctionsId,[],["1"]},
		     {'SwM',
		      [],
		      [{swMId,[],["1"]}]}]}]},
ct:pal("Get_config ~n~p",[Get_config]),

    %% {ok,_} = netconf_open(nc1,[]),
    %% case ct_netconfc:get_config(nc1,running,Get_config) of
    case netconf(get_config, [NC_Session, running, Get_config]) of
	{ok, [{'ManagedElement',
	       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	       [{managedElementId,[],[MeId]},
		{'SystemFunctions',[],
		 [{systemFunctionsId,[],["1"]},
		  {'SwM',
		   _,
		   Packages}]}]}]} ->
%eransbn
	   %% ct:pal("Packages ~n~p",[Packages]),
	    
%%ct:pal("eransbn Packages ~p",[ [Label ||{'UpgradePackage',[],[{upgradePackageId,[],[Label]}|_]} <- Packages]]),
%eransbn
	    [Label ||{'UpgradePackage',[],[{upgradePackageId,[],[Label]}|_]} <- Packages];
	    %% ct_netconfc:close_session(nc1),
	    %% Labels;
	{ok, Reply} ->
	    %% ct_netconfc:close_session(nc1),
	    ct:pal("Reply = ~p~n",[Reply]),
	    ct:fail("Unexpected netconf Reply");
    	{error, Error} ->
	    %% ct_netconfc:close_session(nc1),
	    ct:pal("get_config Error = ~p~n",[Error]),
	    ct:fail("get_config Error")
    end.


%%%--------------------------------------------------------------------
%%% Description: Preforms one of the UP actions prepare, verify, activate,
%%%              commit and confirm.
%%%--------------------------------------------------------------------

up_action_generic(_Config, UpAction, Label, MeId, NC_Session) ->
    ct:pal("Execution action ~w",[UpAction]),
    %% MeId = proplists:get_value(meId, Config),
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
    %% {ok, _} = netconf_open(nc1, []),
    %% case ct_netconfc:action(nc1,  Action) of

    Start1 = os:timestamp(),

    case netconf(action, [NC_Session, Action]) of
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
    %% ct_netconfc:close_session(nc1),

    %% case UpAction of
    %% 	activate ->
    %% 	    ct:pal("Check that the coli connection goes down"),
    %% 	    poll_connection(coli, 1000, {error,econnrefused}, 180),

    %% 	    ct:pal("Wait for the coli connection"),
    %% 	    %% poll_connection(coli, 3000, ok, 60);
    %% 	    poll_connection(coli, 1000, ok, 60);
    %% 	_ ->
    %% 	    ok
    %% end,

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
    %% ct:pal("Time: ~p~n",[Time]),
    Time.


%%%--------------------------------------------------------------------
%%% Description: Read the action id of the last progress report to prevent
%%%              reading old reports
%%%--------------------------------------------------------------------

get_action_id(ProgressFilter) ->

    %% {ok, _} = netconf_open(nc1, []),
    %% {ok, A} = ct_netconfc:get(nc1, ProgressFilter),
    %% ct_netconfc:close_session(nc1),
    {ok, A} = netconf(get, [nc1, ProgressFilter]),
    %% ct:print("get_action_id~n~p~n",[A]),
    case extract_element(reportProgress, A) of
	{ok, {reportProgress, KeyList, []}} ->
	    % the {unset, "true"} tuple may occur anywhere
	    % in the keylist; erarafo R4A/2
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
    %% Get = {'ManagedElement',
    %% 	   [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
    %% 	   [{managedElementId, [], [MeId]},
    %% 	    {release, []}]},
    %% {ok, R} = netconf(get, [nc1, Get]),
    %% {ok, {_, _, [Current]}} = extract_element(release, R),

    %% case Mode of
    %% 	switch when Current == Release ->
    %% 	    ct:pal("Current release is still: ~p~n",
    %% 		   [Current, Release]),
    %% 	    ct:fail({release_not_updated, Current});
    %% 	keep when Current /= Release ->
    %% 	   ct:pal("Current release: ~p~nExpected release: ~p~n",
    %% 		  [Current, Release]),
    %% 	   ct:fail({wrong_release, Current});
    %%    _ ->
    %% 	   ok
    %% end.


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
	    wait_for_progress(Attribute, OldActionId, ProgressFilter);
	{error, Reason} ->
	    ct:log("~p~n",[Reason]),
	    timer:sleep(?Sleep),
	    wait_for_progress(Attribute, OldActionId, ProgressFilter)
    end.

loop_wait_for_progress(Attribute, OldActionId, ProgressFilter) ->
   %% ct:pal("eransbn loop_wait_for_progress ~p",[Attribute]),
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
%%ct:pal("loop_wait_for_progress Attribute ~p",[Attribute]),
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
    ct:pal("Attribute: ~p~nProgressReport:~p",[Attribute, ProgressReport]),
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

format_progress_report({reportProgress, _, Members}) ->
    [io_lib:format("reportProgress:~n",[])|format_progress_report(Members)];
%% format_progress_report([{additionalInfo, _, [Value]}|Members]) ->
%%     [io_lib:format("additionalInfo:~n   ~s~n",[hd(Value)])|
%%      [io_lib:format("   ~s~n",[X])||X<-tl(Value)]]++
%% 	format_progress_report(Members);
format_progress_report([{Key, _, [Value]}|Members]) ->
    [io_lib:format("~w: ~s ~n",[Key, Value])|
     format_progress_report(Members)];
format_progress_report([{Key, _, []}|Members]) ->
    [io_lib:format("~w: ~s ~n",[Key, ""])|
     format_progress_report(Members)];
format_progress_report([]) -> [];
format_progress_report(X) ->
    ct:pal("~p~n",[X]),
    ct:fail(unknown).


%%%--------------------------------------------------------------------
%%% Description: Netconf
%%%--------------------------------------------------------------------

netconf(F, A) ->
    %% %% {ok, _} = netconf_open(nc1, []),
    %% Wait for netconf is ready to be used.
    swm_test_lib:check_nc_session(nc1),
    Res = apply(ct_netconfc, F, A),
    ok = ct_netconfc:close_session(nc1),
    %% ct:pal("netconf/2 result: ~p", [Res]),
    Res.

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
get_latest_up(Config, NC_Session) ->
     ct:log("Config:~n~p~n",[Config]),
    %% case ?config(saved_config, Config) of
    %% 	{_Saver, ConfigList} ->
    %% 	    ct:log("Saved ConfigList: ~p",[ConfigList]),
    %% 	    {uplabel, Label} = lists:keyfind(uplabel, 1, ConfigList),
    %% 	    Label;
    %% 	undefined ->
    %% 	    UpList = get_ups(Config, NC_Session),
    %% 	    ct:log("UpList: ~p",[UpList]),
    %% 	    [Label | _] = lists:reverse(UpList), % Latest UP
    %% 	    ct:log("UP does not exist in Config, try last UP from UP list" ,[]),
    %% 	    %% test_server:break("A"),
    %% 	    Label
    %% end,
  case proplists:get_value(uplabel, Config) of
    	
    	undefined ->
    	    UpList = get_ups(Config, NC_Session),
    	    ct:log("UpList: ~p",[UpList]),
    	    [Label | _] = lists:reverse(UpList), % Latest UP
    	    ct:log("UP does not exist in Config, try last UP from UP list" ,[]),
    	    %% test_server:break("A"),
	  Label;
      Label ->
    	    ct:log("Saved UpList: ~p",[Label]),
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
    %% ct:pal("AllAttr: ~p",[AllAttr]),
    %% Filter out reportProgress
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





%%% Action = string() to match the action from progress.
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

	%% _Action -> %% prepare, verify, activate and confirm
	%%     %% Get UP progress report
	%%     {reportProgress,
	%%      [{struct,"AsyncActionProgress"}],
	%%      ProgressReport} = get_report_progress(Session, MeId),
	%%     ProgressReport
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
	    %% case Node of
	    %% 	dummy ->
	    %% 	    ok;
	    %% 	_ ->
	    %% 	    check_node_state(Node)
	    %% end,
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

	    %% ct:pal("UPInfoList:  ~p ",[UPInfoList]),
	    %% Filter out reportProgress

	    ct:log("UPInfoList:~p",[UPInfoList]),
	    %% {reportProgress,[_], ReportProgress } =
	    %% 	lists:keyfind(reportProgress, 1, UPInfoList),
	    %% ct:pal("### UP_ReportProgress: ~p", [ReportProgress]),
	    %% ReportProgress;
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
%%% Description: wait forFINISHED in  progress results and return results.
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

%% %%%--------------------------------------------------------------------
%% %%% Description: check_node_state
%% %%% Node = ErlNode
%% %%%--------------------------------------------------------------------
%% check_node_state(Node) ->
%%     check_node_state(Node, 600000). %% 10 min

%% check_node_state(Node, Timeout) when Timeout < 0 ->
%%     ct:pal("ErlNode not up : ~p", [Node]),
%%     ct:fail("Node not up within max timeout.");

%% check_node_state(Node, Timeout) ->
%%     case net_adm:ping(Node) of
%% 	pang ->
%% 	    ct:pal("Nodedown. ~n"
%%     		   "sleep and try again."),
%%     	    timer:sleep(10000),
%%     	    check_node_state(Node, Timeout-10000);
%% 	_Res ->
%% 	    ct:pal("Ping res: ~p", [_Res]),
%% 	    ok
%%     end.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
perform_ug_action(Action, _Config, Label, MeId, NC_Session) ->
    %% %%%%
    %% %% Get managedElementId
    %% %%%%
    %% MeId = swm_test_lib:get_me_id(NC_Session),

    %% %%%%
    %% %% Get Latest UP
    %% %%%%
    %% Label = get_latest_up(Config),

    Start1 = os:timestamp(),
    %%%%
    %% Perform ug action
    %%%%
    ct:pal("Perform ug action:~n~p~n",[Action]),
    swm_test_lib:ug_action_match_result(NC_Session,
    					"SUCCESS",
    					Label,
    					MeId,
    					Action),

    End1 = os:timestamp(),

    Time = trunc(timer:now_diff(End1, Start1) / 1000/1000),
    %% ct:pal("Time: ~p~n",[Time]),
    Time.
    %% ok.


%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_basic_test_SUITE.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R5A/R6A/R7A/R11A/1
%%%
%%% @doc == Basic test suite for SWM==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% @end

-module(swm_basic_test_SUITE).
-vsn('/main/R2A/R3A/R5A/R6A/R7A/R11A/1').
-author('etxjotj').

%%% ----------------------------------------------------------
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% R2A/1      2014-05-13 etxjotj     Created
%%% R2A/2      2014-07-29 etxjotj     Included upgrade test
%%% R3A/1      2015-02-28 etxkols     Preparation for cluster
%%% R3A/2      2015-02-28 etxkols     Preparation for 2 labs
%%% R3A/3      2015-07-10 etxjovp     Add group definitions used by CS CI
%%% R6A/1      2016-08-22 etxkols     Git migration requires that CC paths is not used 
%%% R7A/1      2016-09-06 etxpejn     Changed random to rand
%%%--------------------------------------------------------------------


-include_lib("common_test/include/ct.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-include("RcsSwM.hrl").
-include("RcsBrM.hrl").
-include("comte_types.hrl").
-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
         groups/0]).
-export([mom_check/1, esi/1]).
%% -export([create/1, prepare/1, verify/1, activate/1, confirm/1]).
%% -export([clean_up/1]).

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    %% run_upgrade is not working at the moment
    %% [mom_check, run_upgrade].
    [mom_check, esi].
%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf.
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {minutes, 10}},
     {ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc1},
		 {rct_netconf,{nc1, html}},
                 {cth_conn_log,[]},
                 {rct_core,[]},
                 {rct_logging,
		  {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
				  %% This filter here should be unnecessary
				  %% But it seems the log scan gets
				  %% contanimated with printouts from earlier
				  %% testcases
				  ["Program ID [0-9]+ has terminated"]}}]}}
    ]}].

%% @hidden
init_per_suite(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    os:cmd(["chmod a+rwx ", RootDir]),
    MeData = rct_rpc:call(rpc1, comsaI, get_managed_element_data, [], 10000),
    MeId =
	case proplists:get_value(networkManagedElementId, MeData) of
	    undefined ->
		"1";
	    NMEI -> NMEI
	end,
    [{host, SftpHost},{username, Username},{password, Password}] = 
	ct:get_config(sftp_server),
    [{meId, MeId},
     {sftp_host, SftpHost},
     {sftp_user, Username},
     {sftp_pass, Password},
     %% {sftp_host, "10.68.200.11"},
     %% {sftp_user, "mauve"},
     %% {sftp_pass, "dilbert"},
     {sftp_root, RootDir} | Config].
%% @hidden
end_per_suite(_Config) ->
    call(swmLib, erase_variable, [swm_basic_tests_return_pid]),
    ok.
%% @hidden
%% init_per_testcase(run_upgrade, Config) ->
%%     ct:print("Now executing ~w~n",[run_upgrade]),
%%     call(swmServer, start_test_event_server, []),
%%     {ok, TmpUpDir} = call(swmOs, recreate_up, [active]),
%%     {ok, TmpUpFile} = call(swmOs, modify_up_version, [TmpUpDir]),
%%     {ConfigurationE, []} = call(xmerl_scan, file, [TmpUpFile]),
%%     ProductE = find_element(product, ConfigurationE), 
%%     PId = find_attribute(id, ProductE),
%%     PVsn = find_attribute(version, ProductE),
%%     Label = add_slash(PId)++"-"++PVsn,
%%     ct:pal("New label is ~p~n",[Label]),
%%     [{tmpup, TmpUpFile},{label, Label}|Config];
init_per_testcase(TestCase, Config) ->
    ct:print("Now executing ~w~n",[TestCase]),
    Config.
%% @hidden
%% end_per_testcase(run_upgrade, Config) ->
%%     TmpUpFile = proplists:get_value(tmpup, Config),
%%     Dir = filename:dirname(TmpUpFile),
%%     call(os, cmd, [["rm -rf ",Dir]]),
%%     call(swmServer, stop_test_event_server, []),
%%     ok;
end_per_testcase(_TestCase, _Config) ->
    ok.

%% @hidden
groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], []},  
     {sbc__upgrade__all__1__group, [], []},  
     {sdc__cover__sim__1__group, [], []},
     {sdc__def__all__1__group, [], []},  
     {sdc__qual__all__1__group, [], []}  
   ].


%% add_slash([$_|T]) ->
%%     add_slash([$/|T]);
%% add_slash([H|T]) ->
%%     [H|add_slash(T)];
%% add_slash([]) -> [].





%%--------------------------------------------------------------------
%% @doc Check that all SWM models are accessible
%% @end

mom_check(Config)->
    MeId = proplists:get_value(meId, Config),
    BrM = 
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'BrM',
	     [{brMId,[],["1"]}]}]}]},
    SwM = 
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'SwM',
	     [{swMId,[],["1"]}]}]}]},
    SwIM = 
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'SwInventory',
	     [{swInventoryId,[],["1"]}]}]}]},
    netconf(get_config, [nc1, running, BrM]),
    netconf(get_config, [nc1, running, SwM]),
    netconf(get_config, [nc1, running, SwIM]).

%%--------------------------------------------------------------------
%% @doc Check that the SWM part of the ESI can be collected correctly
%% @end     
%%--------------------------------------------------------------------

esi(_) ->
    call(swmI, generate_esi, []).


%% @doc run_upgrade
%% @end

%% run_upgrade(Config) ->
%%     clean_up(Config),
%%     create(Config),
%%     prepare(Config),
%%     verify(Config),
%%     activate(Config),
%%     confirm(Config).

%%--------------------------------------------------------------------
%% doc create upgrade package using a temporary up 
%% The temporary up is created in the preparations for the run_upgrade
%% testcase
%% end

%% create(Config) ->
%%     call(swmLib, set_variable, [swm_basic_tests_return_pid, self()]),
%%     TmpUpFile = proplists:get_value(tmpup, Config),
%%     DnRev = make_dn_rev("ManagedElement=1,SystemFunctions=1,SwM=1"),
%%     Url = "file://localhost:0"++TmpUpFile,
%%     Params = [{<<"uri">>, ?STRING(list_to_binary(Url))}],
%%     TxId = rand:uniform(10000),
%%     Args = [<<"createUpgradePackage">>, DnRev, Params, TxId],
%%     ct:pal("Sending command: ~p~n",[Args]),
%%     ActionId = case call(swmModel, action, Args) of
%% 		   {?UINT16,0} ->
%% 		       ct:fail(create_action_failed);
%% 		   {?UINT16,AId} -> 
%% 		       AId
%% 	       end,
%%     ct:pal("ActionId = ~p~n",[ActionId]),
%%     case wait_for_progress(swM, createUpgradePackage) of
%% 	"SUCCESS" ->
%% 	    ok;
%% 	Other ->
%% 	    ct:fail({create_result, Other})
%%     end.

%%--------------------------------------------------------------------
%% doc Send prepare action
%% end

%% prepare(Config) ->
%%     call(swmLib, set_variable, [swm_basic_tests_return_pid, self()]),
%%     Label = proplists:get_value(label, Config),
%%     up_action_generic(prepare, Label).

%%--------------------------------------------------------------------
%% doc Send verify action
%% end

%% verify(Config) ->
%%     call(swmLib, set_variable, [swm_basic_tests_return_pid, self()]),
%%     Label = proplists:get_value(label, Config),
%%     up_action_generic(verify, Label).

%%--------------------------------------------------------------------
%% doc Send activate action
%% end

%% activate(Config) ->
%%     call(swmLib, set_variable, [swm_basic_tests_return_pid, self()]),

%%     [Obj] = call(mnesia, dirty_read, [{swM, {"1","1","1"}}]),
%%     call(mnesia, dirty_write, [Obj#swM{fallbackTimer=300}]),

%%     Label = proplists:get_value(label, Config),
%%     up_action_generic(activate, Label).
%%     %% MeId = proplists:get_value(meId, Config),
%%     %% case is_alarm(MeId, "9175043") of
%%     %% 	true ->
%%     %% 	    ok;
%%     %% 	false ->
%%     %% 	    ct:fail("FallbackOperationStartingSoon alarm was not raised")
%%     %% end.


%%--------------------------------------------------------------------
%% doc Send commit action
%% end

%% confirm(Config) ->
%%     call(swmLib, set_variable, [swm_basic_tests_return_pid, self()]),
%%     Objs = call(ets, tab2list, [upgradePackage]),
%%     [UpKey] = [Obj#upgradePackage.upgradePackageId||
%% 		  Obj<-Objs,
%% 		  Obj#upgradePackage.state == 
%% 		      ?UpgradePackageState_WAITING_FOR_COMMIT],
%%     Label = element(4, UpKey),

%%     UpMoRef = "ManagedElement=1,SystemFunctions=1,SwM=1,UpgradePackage="++Label,
%%     DnRev = make_dn_rev(UpMoRef),
%%     Params = [],
%%     TxId = rand:uniform(10000),
%%     Args = [<<"confirm">>, DnRev, Params, TxId],
%%     ct:pal("Sending command: ~p~n",[Args]),
%%     case call(swmModel, action, Args) of
%% 	{?BOOL,true} ->
%% 	    ok;
%% 	{?BOOL,false} -> 
%% 	    ct:fail({confirm, failed})
%%     end,
%%     MeId = proplists:get_value(meId, Config),
%%     case is_alarm(MeId, "9175043") of
%% 	false ->
%% 	    ok;
%% 	true ->
%% 	    ct:fail("FallbackOperationStartingSoon alarm was not ceased")
%%     end.




%%--------------------------------------------------------------------
%% doc Clean away unused UPs
%% end

%% clean_up(_) ->
%%     call(swmLib, set_variable, [swm_basic_tests_return_pid, self()]),
%%     %% Normally in this context, there should not be more than up, other than
%%     %% during development testing, so we're doing this somewhat crude
%%     DnRev = make_dn_rev("ManagedElement=1,SystemFunctions=1,SwM=1"),

%%     UpKeys = call(mnesia, dirty_all_keys, [upgradePackage]),
%%     [begin
%% 	 TxId = rand:uniform(10000),
%% 	 UpMoRef = 
%% 	     "ManagedElement=1,SystemFunctions=1,SwM=1,UpgradePackage="++Label,
%% 	 Params = [{<<"upgradePackage">>, ?REFERENCE(list_to_binary(UpMoRef))}],
%% 	 Args = [<<"removeUpgradePackage">>, DnRev, Params, TxId],
%% 	 ct:pal("Sending command: ~p~n",[Args]),
%% 	 case call(swmModel, action, Args) of
%% 	     {?BOOL,true} ->
%% 		 ok;
%% 	     {?BOOL,false} -> 
%% 		 ct:fail(remove_action_failed)
%% 	 end,
%% 	 wait_for_progress(swM, removeUpgradePackage)
%%      end||{_, _, _, Label}<-UpKeys],
%%     ok.




%%%--------------------------------------------------------------------
%%% LIBRARY FUNCTIONS
%%%--------------------------------------------------------------------

%%%--------------------------------------------------------------------
%%% Description: Execute asyncrnous upgrade package actions
%%%--------------------------------------------------------------------


%% up_action_generic(UpAction, Label) ->
%%     ct:pal("Executing action ~w",[UpAction]),
%%     UpMoRef = "ManagedElement=1,SystemFunctions=1,SwM=1,UpgradePackage="++Label,
%%     DnRev = make_dn_rev(UpMoRef),
%%     Params = [],
%%     TxId = rand:uniform(10000),
%%     Args = [list_to_binary(atom_to_list(UpAction)), DnRev, Params, TxId],
%%     ct:pal("Sending command: ~p~n",[Args]),
%%     case call(swmModel, action, Args) of
%% 	{?BOOL,true} ->
%% 	    ok;
%% 	{?BOOL,false} -> 
%% 	    ct:fail({UpAction, failed})
%%     end,
%%     case wait_for_progress(upgradePackage, UpAction) of
%% 	"SUCCESS" ->
%% 	    ok;
%% 	WfPResult ->
%% 	    ct:pal("~w: ~s",[UpAction, WfPResult]),
%% 	    ct:fail(WfPResult)
%%     end.

		    
%% upgradePackageState(?UpgradePackageState_INITIALIZED) ->
%%     "INITIALIZED";
%% upgradePackageState(?UpgradePackageState_PREPARE_IN_PROGRESS) ->
%%     "PREPARE_IN_PROGRESS";
%% upgradePackageState(?UpgradePackageState_PREPARE_COMPLETED) ->
%%     "PREPARE_COMPLETED";
%% upgradePackageState(?UpgradePackageState_ACTIVATION_IN_PROGRESS) ->
%%     "ACTIVATION_IN_PROGRESS";
%% upgradePackageState(?UpgradePackageState_ACTIVATION_STEP_COMPLETED) ->
%%     "ACTIVATION_STEP_COMPLETED";
%% upgradePackageState(?UpgradePackageState_WAITING_FOR_COMMIT) ->
%%     "WAITING_FOR_COMMIT";
%% upgradePackageState(?UpgradePackageState_COMMIT_COMPLETED) ->
%%     "COMMIT_COMPLETED";
%% upgradePackageState(?UpgradePackageState_DEACTIVATION_IN_PROGRESS) ->
%%     "DEACTIVATION_IN_PROGRESS".

%%%--------------------------------------------------------------------
%%% Description: Netconf
%%%--------------------------------------------------------------------

netconf(F, A) ->
    open(nc1, []),
    case apply(ct_netconfc, F, A) of
	ok ->
	    ok = ct_netconfc:close_session(nc1),
	    ok;
	{ok, Res} ->
	    ok = ct_netconfc:close_session(nc1),
	    {ok, Res};
	Other ->
	    ct:fail(Other)
    end.

open(Nc, Opts) ->
    case ct_netconfc:open(Nc, Opts) of
	{ok, _}  -> ok;
	{error, Reason} ->
	    ct:print("Netconf open ~p~n",[{error, Reason}]),
	    open(Nc, Opts, 120)
    end.
		
open(Nc, Opts, N) when N > 0 ->
    timer:sleep(1000),
    case ct_netconfc:open(Nc, Opts) of
	{ok, _}  -> ok;
	{error, Reason} ->
	    ct:print("Netconf open ~p~n",[{error, Reason}]),
	    open(Nc, Opts, N-1)
    end;
open(Nc, Opts, _) ->
    timer:sleep(1000),
    case ct_netconfc:open(Nc, Opts) of
	{ok, _} -> ok;
	{error, Reason}->
	    ct:pal("Netconf open failed ~p~n",[{error, Reason}]),
	    ct:fail({error, Reason})
    end.

    


%%%--------------------------------------------------------------------
%%% Description: Make an rpc call to the node
%%%--------------------------------------------------------------------

call(M, F, A) ->
    rct_rpc:call(rpc1, M, F, A, 120000, noprint).

%%%--------------------------------------------------------------------
%%% Description: Tell if an alarm is present or not
%%%--------------------------------------------------------------------

%% is_alarm(MeId, MinorType) ->
%%     FmAlarms = get_all_alarms(MeId),
%%     lists:foldr(
%%       fun(_FmAlarm, true) ->
%% 	      true;
%% 	 (FmAlarm, false) ->
%% 	      case extract_element(minorType, [FmAlarm]) of
%% 		  {ok, {_, _, [MinorType]}} ->
%% 		      ct:pal("FmAlarm MinorType : ~p, Exist.", [MinorType]),
%% 		      true;
%% 		  _ ->
%% 		      ct:pal("FmAlarm MinorType : ~p, Not Exist.", [MinorType]),
%% 		      false
%% 	      end
%%       end, false, FmAlarms).

%% get_all_alarms(MeId) ->
%%     Get = {'ManagedElement',
%% 	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
%% 	   [{managedElementId,[],[MeId]},
%% 	    {'SystemFunctions',
%% 	     [{systemFunctionsId,[],["1"]},
%% 	      {'Fm',
%% 	       [{fmId,[],["1"]}]}]}]},
%%     {ok, Result} = netconf(get, [nc1, Get]),
%%     {ok, {_, _, Contents}} = extract_element('Fm', Result),
%%     [FmAlarmE||FmAlarmE<-Contents,
%% 	       element(1, FmAlarmE) == 'FmAlarm'].

%%%--------------------------------------------------------------------
%%% Description: From a general short xml syntax, extract a certain xml
%%%              element
%%%--------------------------------------------------------------------
%% extract_element(Element, [{Element, Attribute, Content}|_]) ->
%%     {ok, {Element, Attribute, Content}};
%% extract_element(Element, [{Element, Content}|_]) ->
%%     {ok, {Element, Content}};
%% extract_element(Element, [{_, _, Content}|Elements]) ->
%%     case extract_element(Element, Content) of
%% 	{ok, Value} ->
%% 	    {ok, Value};
%% 	not_found ->
%% 	    extract_element(Element, Elements)
%%     end;
%% extract_element(Element, [{_, Content}|Elements]) ->
%%     case extract_element(Element, Content) of
%% 	{ok, Value} ->
%% 	    {ok, Value};
%% 	not_found ->
%% 	    extract_element(Element, Elements)
%%     end;
%% extract_element(Element, [_|Elements]) ->
%%     extract_element(Element, Elements);
%% extract_element(_, []) ->
%%     not_found.

%%%--------------------------------------------------------------------
%%% Description: Make dn rev
%%%--------------------------------------------------------------------

%% make_dn_rev(MoRef) ->
%%     case [list_to_binary(X)||X<-string:tokens(MoRef, ",=")] of
%% 	[<<"ManagedElement">>, _ |Tail] ->
%% 	    MeData = call(comsaI, get_managed_element_data, []),
%% 	    MeId = 
%% 		case proplists:get_value(networkManagedElementId, MeData) of
%% 		    undefined ->
%% 			"1";
%% 		    NMEI -> NMEI
%% 		end,

%% 	    lists:reverse([<<"ManagedElement">>, list_to_binary(MeId)|Tail]);
%% 	_ ->
%% 	    ct:fail({incomplete_moref, MoRef})
%%     end.

%%%--------------------------------------------------------------------
%%% Description: Wait for progress
%%%--------------------------------------------------------------------
%% wait_for_progress(Source, Action) when is_atom(Action) ->
%%     wait_for_progress(Source, atom_to_list(Action));
%% wait_for_progress(Source, Action) when is_list(Action)->
%%     receive 
%% 	{mnesia_table_event, {write, Source, Obj, _, _}} ->
%% 	    case Obj of 
%% 		_ when Source == swM ->
%% 		    wait_for_progress(
%% 		      Source, Action, Obj#swM.reportProgress);
%% 		_ when Source == upgradePackage ->
%% 		    wait_for_progress(
%% 		      Source, Action, Obj#upgradePackage.reportProgress)
%% 	    end;
%% 	{mnesia_table_event, _} ->
%% 	    wait_for_progress(Source, Action)
%%     after 15000 ->
%% 	    call(swmLib, set_variable, [swm_basic_tests_return_pid, self()]),
%% 	    wait_for_progress(Source, Action)
%% 	    %% case Source of
%% 	    %% 	swM ->
%% 	    %% 	    [Obj] = call(mnesia, dirty_read, [{swM, {"1","1","1"}}]),
%% 	    %% 	    wait_for_progress(Source, Obj#swM.reportProgress);
%% 	    %% 	upgradePackage ->
%% 	    %% 	    Label = find_up(),
%% 	    %% 	    Args = [{upgradePackage, {"1","1","1",Label}}],
%% 	    %% 	    [Obj] = call(mnesia, dirty_read, Args),
%% 	    %% 	    wait_for_progress(Source, Obj#upgradePackage.reportProgress)
%% 	    %% end
%%     end.

%% wait_for_progress(swM, Action, ProgressReport) 
%%   when is_record(ProgressReport, 'AsyncActionProgress'),
%%        ProgressReport#'AsyncActionProgress'.actionName == Action ->	
%%     case ProgressReport#'AsyncActionProgress'.state of
%% 	?ActionStateType_FINISHED ->
%% 	    ct:pal("~s~n",[format_progress_report(ProgressReport)]),
%% 	    actionResultType(ProgressReport#'AsyncActionProgress'.result);
%% 	?ActionStateType_CANCELLED ->
%% 	    ct:pal("~s~n",[format_progress_report(ProgressReport)]),
%% 	    actionResultType(ProgressReport#'AsyncActionProgress'.result);
%% 	CurrentState ->
%% 	    Current = actionStateType(CurrentState),
%% 	    Percent = ProgressReport#'AsyncActionProgress'.progressPercentage,
%% 	    Info = ProgressReport#'AsyncActionProgress'.progressInfo,
%% 	    ct:pal("State: ~s ~w % ready~n~s",[Current, Percent, Info]),
%% 	    wait_for_progress(swM, Action)
%%     end;
%% wait_for_progress(upgradePackage, Action, ProgressReport) 
%%   when is_record(ProgressReport, 'AsyncActionProgressWithSteps'),
%%        ProgressReport#'AsyncActionProgressWithSteps'.actionName == Action ->	
%%     case ProgressReport#'AsyncActionProgressWithSteps'.state of
%% 	?ActionStateType_FINISHED ->
%% 	    ct:pal("~s~n",[format_progress_report(ProgressReport)]),
%% 	    actionResultType(
%% 	      ProgressReport#'AsyncActionProgressWithSteps'.result);
%% 	?ActionStateType_CANCELLED ->
%% 	    ct:pal("~s~n",[format_progress_report(ProgressReport)]),
%% 	    actionResultType(
%% 	      ProgressReport#'AsyncActionProgressWithSteps'.result);
%% 	CurrentState ->
%% 	    Current = actionStateType(CurrentState),
%% 	    Percent = ProgressReport#'AsyncActionProgressWithSteps'.progressPercentage,
%% 	    Info = ProgressReport#'AsyncActionProgressWithSteps'.progressInfo,
%% 	    ct:pal("State: ~s ~w % ready~n~s",[Current, Percent, Info]),
%% 	    wait_for_progress(upgradePackage, Action)
%%     end;
%% wait_for_progress(Source, Action, undefined) ->
%%     wait_for_progress(Source, Action);
%% wait_for_progress(Source, Action, _) ->
%%     wait_for_progress(Source, Action).
    
%% actionStateType(?ActionStateType_CANCELLING) -> "CANCELLING";
%% actionStateType(?ActionStateType_RUNNING) -> "RUNNING";
%% actionStateType(?ActionStateType_FINISHED) -> "FINISHED";
%% actionStateType(?ActionStateType_CANCELLED) -> "CANCELLED".

%% actionResultType(?ActionResultType_SUCCESS) -> "SUCCESS";
%% actionResultType(?ActionResultType_FAILURE) -> "FAILURE";
%% actionResultType(?ActionResultType_NOT_AVAILABLE) -> "NOT_AVAILABLE".


%% format_progress_report(ProgressReport) 
%%   when is_record(ProgressReport, 'AsyncActionProgress') ->
%%     Values = 
%% 	[{X, [], [case Y of 
%% 		      Y when X == state ->
%% 			  actionStateType(Y);
%% 		      Y when X == result ->
%% 			  actionResultType(Y);
%% 		      Y when is_integer(Y) ->
%% 			  integer_to_list(Y);
%% 		      Y -> Y
%% 		  end]}||
%% 	    {X,Y}<-
%% 		lists:zip(
%% 		  [struct|record_info(fields, 'AsyncActionProgress')],
%% 		  tuple_to_list(ProgressReport))],
%%     format_progress_report(Values);
%% format_progress_report(ProgressReport) 
%%   when is_record(ProgressReport, 'AsyncActionProgressWithSteps') ->
%%     Values = 
%% 	[{X, [], [case Y of 
%% 		      Y when X == state ->
%% 			  actionStateType(Y);
%% 		      Y when X == result ->
%% 			  actionResultType(Y);
%% 		      Y when is_integer(Y) ->
%% 			  integer_to_list(Y);
%% 		      Y -> Y
%% 		  end]}||
%% 	    {X,Y}<-
%% 		lists:zip(
%% 		  [struct|record_info(fields, 'AsyncActionProgressWithSteps')],
%% 		  tuple_to_list(ProgressReport))],
%%     format_progress_report(Values);
%% format_progress_report({reportProgress, _, Members}) ->
%%     [io_lib:format("reportProgress:~n",[])|format_progress_report(Members)];
%% format_progress_report([{additionalInfo, _, [Value]}|Members]) ->
%%     [io_lib:format("additionalInfo:~n   ~s~n",[hd(Value)])|
%%      [io_lib:format("   ~s~n",[X])||X<-tl(Value)]]++
%% 	format_progress_report(Members);
%% format_progress_report([{Key, _, [Value]}|Members]) ->
%%     [io_lib:format("~w: ~s ~n",[Key, Value])|
%%      format_progress_report(Members)];
%% format_progress_report([{Key, _, []}|Members]) ->
%%     [io_lib:format("~w: ~s ~n",[Key, ""])|
%%      format_progress_report(Members)];
%% format_progress_report([]) -> [];
%% format_progress_report(X) -> 
%%     ct:pal("~p~n",[X]),
%%     ct:fail(unknown).
			      
%% %%% ----------------------------------------------------------
%% %%% #           find_element(ElementName, Element)
%% %%% #           find_element(ElementName, Content)
%% %%% Input: ElementName:atom()
%% %%%        Element:#xmlElement{} or
%% %%%        Content.[#xmlElement{}] a list of elements
%% %%% Output: #xmlElement{}
%% %%% Exceptions: A badmatch occurs if the element does not exist
%% %%% Description: Finds a sub element to an xml element, or in a list
%% %%%              of element contents. Assumes there is only one element
%% %%%              with the same name
%% %%% ----------------------------------------------------------

%% find_element(ElementName, Element) when is_record(Element, xmlElement) ->
%%     find_element(ElementName, Element#xmlElement.content);
%% find_element(ElementName, ContentList) ->
%%     case lists:keysearch(ElementName, #xmlElement.name, ContentList) of
%% 	{value, Element}  ->
%% 	    Element;
%% 	false ->
%% 	    throw({not_found, ElementName})
%%     end.


%%% ----------------------------------------------------------
%%% #           find_attribute(AttributeName, Element)
%%% #           find_attribute(AttributeName, AttributeList)
%%% Input: AttributeName:atom()
%%%        Element:#xmlElement{} or
%%%        AttributeList:[#xmlattribute{}] a list of xml attributes
%%% Output: Value:string()
%%% Exceptions: erlang:error invoked if attribute does not exist
%%% Description: Finds an attribute to an xml element, or in a list of
%%%              attributes and returns the value of the attribute
%%% ----------------------------------------------------------

%% find_attribute(AttributeName, Element) when is_record(Element, xmlElement) ->
%%     find_attribute(AttributeName, Element#xmlElement.attributes);
%% find_attribute(AttributeName, AttributeList) ->
%%     case lists:keysearch(AttributeName, #xmlAttribute.name, AttributeList) of
%%         {value, Attribute} ->
%%             Attribute#xmlAttribute.value;
%%         false ->
%% 	    throw({not_found, AttributeName})
%%     end.

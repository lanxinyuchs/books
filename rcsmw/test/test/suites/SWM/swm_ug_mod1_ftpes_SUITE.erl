-module(swm_ug_mod1_ftpes_SUITE).

%%% ----------------------------------------------------------
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% R9A/1      2017-02-28 eivmiha     create, create_cancel, create_wrong_uri,
%%%                                   create_wrong_password, prepare, prepare_cancel
%%% R10A/3     2017-07-04 eivmiha     removed prepare

%%% ----------------------------------------------------------

-include_lib("common_test/include/ct.hrl").

-export([suite/0,
     init_per_suite/1,
     end_per_suite/1,
     init_per_testcase/2,
     end_per_testcase/2,
     all/0]).

-export([create/1, create_wrong_uri/1, create_wrong_password/1, create_cancel/1,
         prepare/1, prepare_connection_lost/1, prepare_cancel/1, remove_created_up/1]).

%% -define(NC_Session, nc1).
-define(ModScript_Path, rct_cc_git_path:find("RCT_TOP", ["test/suites/SWM/bin/"])).

-define(ModScriptName_1, "swm_mod_fake_cxp.sh").

-define(NC_Session, nc1).
-define(CLI_Session, cli1).
-define(UPGRADE_FOLDER, "SWM_UP").

suite() ->
    [{ct_hooks, [{rct_htmllink,[]},
         {rct_upgrade,ug1},
         {rct_rpc, rpc},
         {rct_core,[]},
                 {rct_logging, {upgrade, [{erlang,{["ERROR REPORT",
                            "CRASH REPORT"],["swmServer: throw euser"]}}]}},
         {rct_cli, {cli1, [manual_connect]}},
         ftpes_hook(),
         {rct_netconf,nc1},
                 {rct_rs232,console},
                 {cth_conn_log,[]}]}].

%% @hidden
init_per_suite(Config) ->
    ct:pal("# init per suite. ~n"
           "Create cxps that shall be used for UG."),
        swm_test_lib:build_new_ug_package(?NC_Session, ug1),
    
    rct_ftpes_client:open(),
    ftpes_test_lib:start_server(rpc),
    NewConfig = ftpes_test_lib:initialize_nc_tc(Config),
    ftpes_test_lib:enable_ftpes_tls(NewConfig),
    ftpes_test_lib:create_test_folder(?UPGRADE_FOLDER),
    {ok, Folder} = rct_ftpes_client:pwd(),
    FilePath = Folder ++ "/",
    Filenames = transfer_files(FilePath),
    [{host, ftpes_test_lib:get_ftpes_test_server_address(ipv4)},
     {username, ftpes_test_lib:get_ftpes_test_server_username()},
     {password, ftpes_test_lib:get_ftpes_test_server_password()}, 
     {file_path, FilePath},
     {filenames, Filenames}
    |NewConfig].

%% @hidden
end_per_suite(Config) ->
    swm_test_lib:step_down_version(ug1),
    delete_files_and_folder(Config),
    ftpes_test_lib:disable_ftpes_tls(),
    ftpes_test_lib:clean_nc_tc(Config),
    rct_ftpes_client:close(),
    ok.

%% @hidden
init_per_testcase(_TestCase, Config) ->
    Up = get_current_up_data(),
    ct:pal("Current UP:~n~p~n",[Up]),
    Config.

%% @hidden
end_per_testcase(_TestCase, Config) ->
    case proplists:get_value(tc_status, Config) of
        ok ->
            ok;
        {failed, Reason}  ->
        ct:pal("Testcase failed due to: ~p.  \n", [Reason])
    end,
    ok.

all() ->
    [create_wrong_uri, create_wrong_password, create, 
     prepare_cancel, remove_created_up].

%%%--------------------------------------------------------------------
%%% @doc Create an upgrade package, using the latest package found in clearcase
%%% @end
%%%--------------------------------------------------------------------

create(Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    
    Host = ?config(host, Config),
    Username = ?config(username, Config),
    Password = ?config(password, Config),
    FilePath = ?config(file_path, Config),
    
    swm_test_lib:ug_create_match_result(?NC_Session,
                        "SUCCESS",
                        Host,
                        Username,
                        Password,
                        FilePath,
                        MeId,
                        ftpes),
    ok.

create_wrong_uri(Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Create UG
    %%%%
    
    Host = ?config(host, Config),
    Username = ?config(username, Config),
    Password = ?config(password, Config),
    
    swm_test_lib:ug_create_match_result(?NC_Session,
                        "FAILURE",
                        Host,
                        Username,
                        Password,
                        "/wronguri",
                        MeId,
                        ftpes),
    ok.

create_wrong_password(Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),
    
    Host = ?config(host, Config),
    Username = ?config(username, Config),
    FilePath = ?config(file_path, Config),
    
    swm_test_lib:ug_create_match_result(?NC_Session,
                        "FAILURE",
                        Host,
                        Username,
                        "wrongpass",
                        FilePath,
                        MeId,
                        ftpes),
    ok.


create_cancel(Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),
    
    Host = ?config(host, Config),
    Username = ?config(username, Config),
    FilePath = ?config(file_path, Config),
    
    ActionId = swm_test_lib:get_swm_action_id(?NC_Session, MeId),
    
    SwVerion = swm_test_lib:get_sw_version(?NC_Session, MeId),
    ct:pal("Active SwVersion : ~p , before create up.",[SwVerion]),

    ct:pal("Execution create upgrade package."),

    Uri = "ftpes://"++Username++"@"++Host++FilePath,

    Action = {'ManagedElement',
          [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
          [{managedElementId, [], [MeId]},
           {'SystemFunctions',
        [{systemFunctionsId,[],["1"]},
         {'SwM',
          [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
          [{swMId,[],["1"]},
           {createUpgradePackage, [],
            [{uri, [Uri]},
             {password, ["wrongpassword"]}]
           }]}]}]},
    {ok, _} = netconf(?NC_Session, action, [Action]),
    
    timer:sleep(10000),
    
    ct:pal("Canceling creation of upgrade package."),
    Action1 = {'ManagedElement',
      [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
      [{managedElementId, [], [MeId]},
       {'SystemFunctions',
    [{systemFunctionsId,[],["1"]},
     {'SwM',
      [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
      [{swMId,[],["1"]},
       {cancel, [], []}]
     }]}]},
    {ok, _} = netconf(?NC_Session, action, [Action1]),
    
    ct:pal("Wait for progress after create UP"),
    wait_for_swm_progress_done(?NC_Session, "FAILURE",
              "createUpgradePackage",
              MeId,
              ActionId,
              dummy),
   
    ok.

%%%--------------------------------------------------------------------
%%% @doc Prepares an upgrade package, which means downloading a complete UP
%%% @end
%%%--------------------------------------------------------------------

prepare(_Config) ->
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
    ct:pal("Perform ug action:~n prepare ~n"),
    swm_test_lib:ug_action_match_result(?NC_Session,
                        "SUCCESS",
                        Label,
                        MeId,
                        prepare),
    ok.

prepare_connection_lost(_Config) ->
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
    ct:pal("Perform ug action:~n prepare ~n"),
    ActionId = swm_test_lib:get_action_id(?NC_Session, MeId),
    
    Key = swm_test_lib:make_key(Label),

    ok = create_action (MeId, Key, prepare, ?NC_Session),
    
    ct:pal("Start check progress."),
    wait_for_progress_done(?NC_Session, "FAILURE", "prepare",
               MeId, ActionId, dummy, Label),
    
    ok.

prepare_cancel(_Config) ->
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
    ct:pal("Perform ug action:~n prepare ~n"),
    ActionId = swm_test_lib:get_action_id(?NC_Session, MeId),
    
    Key = swm_test_lib:make_key(Label),

    ok = create_action (MeId, Key, prepare, ?NC_Session),
    ok = create_action (MeId, Key, cancel, ?NC_Session),
    
    ct:pal("Start check progress."),
    wait_for_progress_done(?NC_Session, "FAILURE", "prepare",
               MeId, ActionId, dummy, Label),
    
    ok.

remove_created_up(_Config) ->
    %%%%
    %% Get Latest UP
    %%%%
    Label = get_latest_up(),

    ok = swm_test_lib:remove_upgrade_package(?NC_Session, Label).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                          HELPER FUNCTIONS                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transfer_files(FilePath) ->
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    {ok, Filenames} = file:list_dir(UGPath),
    
    [apply(fun() -> 
             {File, IoData} = readfile(Filename), 
             ok = rct_ftpes_client:write_file(FilePath ++ File, IoData) 
     end, []) || Filename <- Filenames],
    Filenames.

delete_files_and_folder(Config) ->
    Filenames = (?config(filenames, Config)),
    
    ok = delete_files(Filenames, Config),
    ok = rct_ftpes_client:rmdir(?config(file_path, Config)).

delete_files([], _Config) ->
    ok;

delete_files([Filename|Filenames], Config) ->
    ok = rct_ftpes_client:delete_file(?config(file_path, Config) ++ Filename),
    delete_files(Filenames, Config).

    
readfile(Filename) ->
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    File = UGPath ++ "/" ++ Filename,
    
    {ok, IoData} = file:read_file(File),
    {Filename, IoData}.
    

create_action (MeId, Key, Action, Session) ->
        Action1 =  {'ManagedElement',
               [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
               [{managedElementId, [], [MeId]},
            {'SystemFunctions',
             [{systemFunctionsId,[],["1"]},
              {'SwM',
               [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
               [{swMId,[],["1"]},
            {'UpgradePackage', [],
             [{upgradePackageId, [Key]},
              {Action, [], []}]}]}]}]},

    ct:log("Calling action"),

    case netconf(Session, action, [Action1]) of
        {error, Error} ->
            ct:pal("ActionError = ~p~n",[Error]),
            ct:fail("Action could not be started");
        {ok, A} ->
        %% ct:pal("# ~p",[A]),
            {ok, {returnValue, _, [ActionResult]}} =
                swm_test_lib:extract_element(returnValue, A),
            ct:pal("Action result: ~s",[ActionResult]),
            case ActionResult of
            "true" -> ok;
            "false" ->
                ct:fail("Action could not start")
            end
    end,
    ok.

wait_for_progress_done(NC_Session, ExpResult, ActionName, MeId,
               ActionId, ErlNode, UP_Label) ->
    case UP_Label of
        dummy -> %% Take latest created UP.
            [Label | _] = lists:reverse(swm_test_lib:get_ups(NC_Session)),
        Label;
    _UP_Label ->
        Label = UP_Label,
        ct:pal("Label:~n~p~n",[Label]),
        Label
    end,


    case wait_for_progress_result(NC_Session, MeId, ActionId, ErlNode,
                  ActionName, UP_Label) of
        [{ExpResult, ActionName, ProgressInfo, ResultInfo,
      State, _ProgReport}] ->
            ct:log("result:~p~n"
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

get_current_up_data()->
    ok = rct_cli:connect(?CLI_Session),
    rct_cli:send(?CLI_Session,"configure"),
    {ok ,RecievedData} = 
    rct_cli:send(?CLI_Session,
             "show ManagedElement=1,SystemFunctions=1,SwM=1"),
    ok = rct_cli:disconnect(?CLI_Session),
    string:tokens(RecievedData, "=\r\n ").

get_latest_up() ->

    UPs = swm_test_lib:get_ups(?NC_Session),
    ct:log("UPs:~n~p~n",[UPs]),

    Label = swm_test_lib:get_highest_label(UPs),
    ct:log("Label:~n~p~n",[Label]),
    Label.

wait_for_swm_progress_done(NC_Session, ExpResult, ActionName, MeId,
               ActionId, ErlNode) ->
    case wait_for_progress_result(NC_Session, MeId, ActionId, ErlNode,
                 ActionName) of
        [{ExpResult, ActionName, ProgressInfo, ResultInfo,
      State, _ProgReport}] ->
            ct:log("result:~p~n"
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

%% %% Return sim,not_sec_card or sec_card
check_kdu()->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    case ct:get_config({list_to_atom(Hw),secure_board}) of
    "yes" ->
        sec_card;
    _Other ->
        not_sec_card
    end.

netconf(Session, F, A) ->
    netconf_open(Session, []),
    Res = apply(ct_netconfc, F, [Session | A]),
    ct_netconfc:close_session(Session),
    Res.

netconf_open(Session, Param)->
    case check_kdu()  of
    TARGET when TARGET == sim;
            TARGET == not_sec_card -> ct_netconfc:open(Session,Param);
    sec_card ->  ct_netconfc:open(Session, [{user, "SysAdminTest"}, {password, "SysAdminTest"}|Param])
    end.

wait_for_progress_result(Session, MeId, OldActionId, Node, Action, UP_Label) ->
    netconf_open(Session, [{timeout, 30000}]),
    wait_for_progress_result(Session, MeId, OldActionId, Node, Action,
                 UP_Label, 3600000).

%%% Action = string() to match the action from progress.
wait_for_progress_result(Session, MeId, OldActionId, Node, Action) ->
    [UP_Label | _] = lists:reverse(swm_test_lib:get_ups(Session)),
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
    ct:pal(" UP_Label je : ~p", [UP_Label]),
    ct:pal(" Action : ~p", [Action]),

    case Node of
    dummy ->
        ok;
    _ ->
        ok = swm_test_lib:check_nc_session(Session, Node)
    end,

    case Action of
    Action when Action == "createUpgradePackage";
            Action == "removeUpgradePackage";
            Action == "swm_progess_report"->
        ct:pal(" check swm prog report ", []),
        {reportProgress,
         [{struct,"AsyncActionProgress"}],
         ProgressReport} = swm_test_lib:get_report_progress(Session, MeId),
        ProgressReport;

    _Action ->
        ProgressReport =  swm_test_lib:get_up_report_progress(Session, MeId, UP_Label),

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
        case swm_test_lib:check_progress_elements(ProgressReport, "CANCELLED") of
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

ftpes_hook() ->
    {rct_ftpes_client, [{port, 21}, {ip, ftpes_test_lib:get_ftpes_test_server_address(ipv4)},
                        {user, ftpes_test_lib:get_ftpes_test_server_username()},
                        {password, ftpes_test_lib:get_ftpes_test_server_password()},
                        {start_session_per_tc, false}]}.

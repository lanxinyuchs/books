%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_FTPES_backup_SUITE.erl %
%%% @author ekurnik
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/R10A/R11A/1
%%%
%%% @doc == Test Suite for testing the transfer ESI from SUT using netconf.==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% @end

-module(swm_FTPES_backup_SUITE).
-vsn('/main/R9A/R10A/R11A/1').
-author('ekurnik').

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
%%% R9A/1      2017-03-31 enekdav     Created
%%% R9A/2      2017-04-03 enekdav     Minor prefix fix
%%% ----------------------------------------------------------
%%% R11A/1     2017-10-05 ekurnik     Fixed autoexport tests
%%%--------------------------------------------------------------------


                        %compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
     init_per_suite/1,
     end_per_suite/1,
     init_per_group/2,
     end_per_group/2,
     init_per_testcase/2,
     end_per_testcase/2,
     all/0,
     groups/0]).
-export([create_backup/1,
     export_backup/1,
     export_backup_slash_name/1,
     export_failure_pass/1,
     export_failure_uri/1,
     export_cancel/1,
     autoexport_backup/1,
     autoexport_failure_pass/1,
     autoexport_failure_uri/1,
     delete_backup/1,
     delete_backup_not_found/1,
     import_backup/1,
     import_failure_pass/1,
     import_failure_uri/1,
     import_backup_cancel/1,
     scheduled_autoexport/1,
     scheduled_autoexport_failure_pass/1,
     scheduled_autoexport_failure_uri/1,
     clean_backups/1,
     restore_backup_before_upgrade/1]).

-export([tc/1]).
-export([create_backup_e/1]).

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [{group, import_backup_group},
     {group, export_backup_group},
     {group, autoexport_backup_group},
     {group, scheduled_backup_group}].

import_backup_group() ->
    [import_backup,
     import_failure_pass,
     import_failure_uri,
     import_backup_cancel].

export_backup_group() ->
    [export_backup,
     export_backup_slash_name,
     export_failure_pass,
     export_failure_uri].
     %%export_cancel].

autoexport_backup_group() ->
    [autoexport_backup,
     autoexport_failure_pass,
     autoexport_failure_uri].

scheduled_backup_group() ->
    [scheduled_autoexport,
     scheduled_autoexport_failure_pass,
     scheduled_autoexport_failure_uri].

-define(DataDir, rct_cc_git_path:find("RCS_TOP", ["SWM/SWM_CNX9012637/test/suites", "SWM/test/suites"])).


%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf.
%% @end
%%--------------------------------------------------------------------
suite() ->
    case os:getenv("SIM_OR_TARGET") of
    "target" ->
        [{timetrap, {minutes, 20}},
         {ct_hooks, 
          case os:getenv("USER") of
          "etxjotj" ->[];
          _ ->  [{rct_rs232, console}]
          end++
          [{rct_htmllink,[]},
           {rct_rpc, rpc_1},
           {rct_netconf,{nc1, html}},
           {rct_netconf,{nc_event, html}},
           {cth_conn_log,[]},
           {rct_power, node},
           {rct_core,[]},
           {rct_logging,
            {all, [{[erlang,swmLog],
                {["ERROR REPORT","CRASH REPORT"],
                 ["Program ID [0-9]+ has terminated",
                   "error: euser",
                   "error: efnamena"]}}]}},
            ftpes_test_lib:ftpes_hook(true)
          ]}];
    _ -> 
        [{timetrap, {minutes, 20}},
         {ct_hooks, [{rct_htmllink,[]},
             {rct_rpc, rpc_1},
             {rct_netconf,{nc1, html}},
             {rct_netconf,{nc_event, html}},
             {cth_conn_log,[]},
             {rct_core,[]},
             {rct_logging,
              {all, [{[erlang,swmLog],
                  {["ERROR REPORT","CRASH REPORT"],
                   ["Program ID [0-9]+ has terminated",
                   "error: euser",
                   "error: efnamena"]}}]}},
              ftpes_test_lib:ftpes_hook(true)
            ]}]
    end.

%%--------------------------------------------------------------------
%% @hidden
init_per_suite(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    os:cmd(["chmod a+rwx ", RootDir]),
    MeData = call(comsaI, get_managed_element_data, []),
    MeId =
    case proplists:get_value(networkManagedElementId, MeData) of
        undefined ->
        "1";
        NMEI -> NMEI
    end,
    Name = "swm_FTPES_backup_SUITE",
    Set =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[MeId]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'BrM',
             [{brMId,[],["1"]},
              {exportPackageLabelPrefix, ["FTPES_Backup"]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Set]),
    [{host, Host},{username, Username},{password, Password}] = 
        ct:get_config(rct_oamap_ipv:sftp_server_iptype()),
    NewConfig = [{meId, MeId},
         {backupName, Name},
         {ftp_host, Host},
         {ftp_user, Username},
         {ftp_pass, Password},
         {ftp_root, RootDir} | Config],
    NewConfig.

%%--------------------------------------------------------------------
%% @hidden
end_per_suite(Config) ->
    MeId = proplists:get_value(meId, Config),
    Set =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[MeId]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'BrM',
             [{brMId,[],["1"]},
              {exportPackageLabelPrefix, [""]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Set]).

%%--------------------------------------------------------------------
%% @hidden
init_per_group(GroupName, Config) when GroupName =:= export_backup_group orelse
                                       GroupName =:= autoexport_backup_group ->
    init_per_group_common(GroupName, Config);
init_per_group(import_backup_group, Config) ->
    NewConfig = init_per_group_common(import_backup_group, Config),
    InitConfig = init_per_testcase_default(export_backup, NewConfig),
    {save_config, ExportConfig} = export_backup(InitConfig),
    call(swmServer, stop_test_event_server, []),
    call(swmLib, erase_variable, [swm_basic_tests_return_pid]),
    File = ?config(delete, ExportConfig),
    clean_backups(ExportConfig),
    ct:print("Init per group complete~n"),
    [{delete, File} | NewConfig];
init_per_group(scheduled_backup_group, Config) ->
    rct_rpc:call(rpc_1, swmLib, set_variable, [scheduler_debug, true], 10000),
    ok = rct_ftpes_client:open(),
    Folder = ftpes_test_lib:create_test_folder("SWM"),
    ok = rct_ftpes_client:cd(".."),
    Started = ftpes_test_lib:start_server(rpc_1),
    NewConfig = ftpes_test_lib:initialize_nc_tc(Config),
    ftpes_test_lib:enable_ftpes_tls(NewConfig),
    ct:print("Init per group complete~n"),
    [{folder, Folder}, {started, Started} | NewConfig].
init_per_group_common(_GroupName, Config) ->
    ok = rct_ftpes_client:open(),
    Folder = ftpes_test_lib:create_test_folder("SWM"),
    ok = rct_ftpes_client:cd(".."),
    call(swmLib, set_variable, [swm_basic_tests_return_pid, self()]),
    call(swmServer, start_test_event_server, []),
    clean_backups(Config),
    call(swmServer, stop_test_event_server, []),
    Started = ftpes_test_lib:start_server(rpc_1),
    NewConfig = ftpes_test_lib:initialize_nc_tc(Config),
    ftpes_test_lib:enable_ftpes_tls(NewConfig),
    [{folder, Folder}, {started, Started} | NewConfig].
%%--------------------------------------------------------------------
%% @hidden
end_per_group(import_backup_group, Config) ->
    ok = rct_ftpes_client:open(),
    ok = rct_ftpes_client:delete_file(?config(delete, Config)),
    end_per_group_common(import_backup_group, Config);
end_per_group(GroupName, Config) ->
    end_per_group_common(GroupName, Config).
end_per_group_common(_GroupName, Config) ->
    ftpes_test_lib:disable_ftpes_tls(),
    case ?config(started, Config) of
        no -> ftpes_test_lib:stop_server(rpc_1);
        yes -> ok
    end,
    ftpes_test_lib:clean_nc_tc(Config),
    
    ok = rct_ftpes_client:open(),
    ok = rct_ftpes_client:rmdir(?config(folder, Config)),
    ok = rct_ftpes_client:close().

%%--------------------------------------------------------------------
%% @hidden
init_per_testcase(TestCase, Config) when TestCase =:= scheduled_autoexport orelse
                                         TestCase =:= scheduled_autoexport_failure_pass orelse
                                         TestCase =:= scheduled_autoexport_failure_uri->
    ct:print("Now executing ~w~n",[TestCase]),
    swm_event_lib:start_subscription(nc_event,[]),
    set_admin_state(Config, "LOCKED"),
    clear_schedule(Config),
    rct_rpc:call(rpc_1, swmLib, set_variable, [swm_basic_tests_return_pid, self()], 10000),
    rct_rpc:call(rpc_1, swmServer, start_test_event_server, [], 10000),
    [{host, Host},{username, Username},{password, Password}] = 
        ftpes_test_lib:get_ftp_config(ftpes),
    Path = ftpes_test_lib:get_ftp_file_path(ftpes, Config),
    Uri = atom_to_list(ftpes) ++ "://"++Username++"@"++Host ++ Path ++ "/" ++ ?config(folder, Config),
    
    [{uri, Uri}, {password, Password}, {log_path, Path} | Config];
init_per_testcase(TestCase, Config) ->
    init_per_testcase_default(TestCase, Config).

%%--------------------------------------------------------------------
init_per_testcase_default(TestCase, Config) ->
    ct:print("Now executing ~w~n",[TestCase]),
    swm_event_lib:start_subscription(nc_event,[]),
    MeId = proplists:get_value(meId, Config),
    set_housekeeping_default(MeId),
    ct:print("Config = ~p~n", [Config]),
    call(swmLib, set_variable, [swm_basic_tests_return_pid, self()]),
    call(swmServer, start_test_event_server, []),
    swmTestLib:fake_vnfm(),
    ok = swmTestLib:wait_for_action_capable_e(),
    [{host, Host},{username, Username},{password, Password}] = 
        ftpes_test_lib:get_ftp_config(ftpes),
    Path = ftpes_test_lib:get_ftp_file_path(ftpes, Config),
    
    Uri = atom_to_list(ftpes) ++ "://"++Username++"@"++Host ++ Path ++ "/" ++ ?config(folder, Config),
    
    [{uri, Uri}, {password, Password}, {log_path, Path}, {host, Host}, {username, Username} | Config].

%%--------------------------------------------------------------------
%% @hidden
end_per_testcase(TestCase = export_backup, Config) ->
    ct:print("Test case ~w complete.~n",[TestCase]),
    swm_event_lib:stop_subscription(),
    call(swmServer, stop_test_event_server, []),
    call(swmLib, erase_variable, [swm_basic_tests_return_pid]),
    ok = rct_ftpes_client:delete_file(?config(delete, ?config(save_config, Config)));
end_per_testcase(TestCase, Config) when TestCase =:= autoexport_backup orelse
                                        TestCase =:= autoexport_failure_pass orelse
                                        TestCase =:= autoexport_failure_uri ->
    ct:print("Test case ~w complete.~n",[TestCase]),
    MeId = proplists:get_value(meId, Config),
    Uri = ?config(uri, Config),
    Password = ?config(password, Config),
    EditAction = {'ManagedElement',  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
                                 [{managedElementId,[],[MeId]},
               {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                 {'BrM', [{brMId,[],["1"]},
                   {'BrmBackupManager', [{brmBackupManagerId,[],["1"]},
                                         {autoExport, ["DISABLED"]},
                                                             {autoExportPassword, [], [{cleartext, [], []}, {password, [], [Password]}]},
                                                             {autoExportUri, [], [Uri]}]}]}]}]},
    netconf(edit_config, [nc1, running, EditAction]),
    ok = swmTestLib:wait_for_action_capable(),
    swm_event_lib:stop_subscription(),
    swmTestLib:flush_mnesia_event_messages(),
    flush_messages(),
    ok;
%% @hidden
end_per_testcase(TestCase, Config) when TestCase =:= scheduled_autoexport orelse
                                        TestCase =:= scheduled_autoexport_failure_pass ->
    ct:print("Ending ~w~n",[TestCase]),
    swm_event_lib:stop_subscription(),
    MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config)++"_sct",
    Set =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[MeId]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'BrM',
             [{brMId,[],["1"]},
              {'BrmBackupManager',
               [{brmBackupManagerId,[],["1"]},
            {'BrmBackupScheduler',
             [{brmBackupSchedulerId,[], ["1"]},
              {scheduledBackupName, [Name]},
          {autoExport, ["DISABLED"]}]}]}]}]}]},
    %% This will cause the system to wait 60 seconds for a go ahead message
    %% before it resets the progress report
    ok = rct_rpc:call(rpc_1, swmLib, set_variable, 
              [backup_test_sched_bu_wait, true], 10000),
    
    ok = netconf(edit_config, [nc1, running, Set]),
    rct_rpc:call(rpc_1, swmLib, erase_variable, [backup_test_sched_bu_wait], 
         10000),
    rct_rpc:call(rpc_1, swmLib, erase_variable, [swm_basic_tests_return_pid], 10000),
    rct_rpc:call(rpc_1, swmServer, stop_test_event_server, [], 10000),
    ok;
%% @hidden
end_per_testcase(TestCase = scheduled_autoexport_failure_uri, _Config) ->
    ct:print("Ending ~w~n",[TestCase]),
    swm_event_lib:stop_subscription(),
    rct_rpc:call(rpc_1, swmLib, erase_variable, [backup_test_sched_bu_wait], 
         10000),
    rct_rpc:call(rpc_1, swmLib, erase_variable, [swm_basic_tests_return_pid], 10000),
    rct_rpc:call(rpc_1, swmServer, stop_test_event_server, [], 10000),
    ok;
%% @hidden
end_per_testcase(TestCase, _Config) ->
    ct:print("Test case ~w complete.~n",[TestCase]),
    swm_event_lib:stop_subscription(),
    call(swmServer, stop_test_event_server, []),
    call(swmLib, erase_variable, [swm_basic_tests_return_pid]),
    ok.

%%--------------------------------------------------------------------
%% @hidden
groups() ->
    ImportBackupGroup = import_backup_group(),
    ExportBackupGroup = export_backup_group(),
    AutoexportBackupGroup = autoexport_backup_group(),
    ScheduledBackupGroup = scheduled_backup_group(),
    [{import_backup_group, [], ImportBackupGroup},
    {export_backup_group, [], ExportBackupGroup},
    {autoexport_backup_group, [], AutoexportBackupGroup},
    {scheduled_backup_group, [], ScheduledBackupGroup},
    {default__group, [], ImportBackupGroup}
    ].

tc(_Config) ->
    swmTestLib:wait_for_action_capable_e().


%%--------------------------------------------------------------------
%% @doc NodeUC441.N Create backup
%% Using netconf to trig a backup to be created.
%% Initiate the action, and then check the model for the expected instance
%% @end

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
    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),
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
    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
    ["SUCCESS"] ->
        ct:pal("Backup succesfully created. Verifying metadata."),
        %% DataDir = proplists:get_value(data_dir, Config), %% This dir is obsolete
        DataDir = ?DataDir,
        ct:pal("DataDir: ~p",[DataDir]),
        SchemaFile = filename:join(DataDir, "backupinfo.xsd"),
        ResultInfo = get_result_info(ProgressFilter),
        BuId = get_buid(ProgressFilter, ResultInfo),
        ct:log("BuId ~p~n", [BuId]),
        BuPath = call(swmLib, backup_dir, [BuId]),
        BuFile = filename:join(BuPath, "backupinfo.xml"),
        {ok, Bin} = call(file, read_file, [BuFile]),
        PrivDir = proplists:get_value(priv_dir, Config),
        MetadataFile = filename:join(PrivDir, "backupinfo.xml"),
        file:write_file(MetadataFile, Bin),
        Res = cmd(["xmllint --schema ", SchemaFile, " ", MetadataFile, " ; echo -n \"Res=$?\""]),
        case lists:last(string:tokens(Res, "\n=")) of
        "0" ->
            ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("createBackup")},
                                   {"CAPABLE", ""}]),
            ok = swmTestLib:wait_for_action_capable(),
            {ok, ResultInfo};
        Other ->
            Msg = xmllint_status(Other),
            ct:pal("xmllint exited with status ~s ~s~n",[Other, Msg]),
            ct:fail({xmllint, Msg})
        end;
    Result ->
        ct:pal("createBackup: ~s~n",[Result]),
        ct:fail(Result)
    end.

create_backup_e(Config) ->
    MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config)++"_e",

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
    ok = swmTestLib:wait_for_action_capable(),    
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

    case wait_for_progress_e(progressReport, ActionId, "BrmBackupManager") of
	{ok, ["SUCCESS"], Fields} ->
	    ct:pal("Backup succesfully created. Verifying metadata."),
	    ct:print("Unused messages ~p~n",[process_info(self(), messages)]),
	    %% DataDir = proplists:get_value(data_dir, Config), %% This dir is obsolete
	    DataDir = ?DataDir,
	    ct:pal("DataDir: ~p",[DataDir]),
	    SchemaFile = filename:join(DataDir, "backupinfo.xsd"),
	    [[ResultInfo]] = proplists:get_value(resultInfo, Fields),
	    %% ResultInfo = get_result_info(ProgressFilter),
	    %% ct:print("get_result_info(~p) = ~p~n",[ProgressFilter, ResultInfo]),
	    BuId = get_buid(ResultInfo),
	    ct:log("BuId ~p~n", [BuId]),
	    BuPath = call(swmLib, backup_dir, [BuId]),
	    BuFile = filename:join(BuPath, "backupinfo.xml"),
	    {ok, Bin} = call(file, read_file, [BuFile]),
	    PrivDir = proplists:get_value(priv_dir, Config),
	    MetadataFile = filename:join(PrivDir, "backupinfo.xml"),
	    file:write_file(MetadataFile, Bin),
	    Res = cmd(["xmllint --schema ", SchemaFile, " ", MetadataFile, " ; echo -n \"Res=$?\""]),
	    case lists:last(string:tokens(Res, "\n=")) of
		"0" ->
		    ok = swmTestLib:check_action_capable_progress(
			   [{"WAIT", 
			     swmTestLib:map_current_action("createBackup")},
			    {"CAPABLE", ""}]),
		    ok = swmTestLib:wait_for_action_capable(),
		    {ok, ResultInfo};
		Other ->
		    Msg = xmllint_status(Other),
		    ct:pal("xmllint exited with status ~s ~s~n",[Other, Msg]),
		    ct:fail({xmllint, Msg})
	    end;
	{ok, Result, _} ->
	    ct:pal("createBackup: ~s~n",[Result]),
	    ct:fail(Result)
    end.

wait_for_progress_e(Attribute, OldActionId, MOC) ->
    receive
	{avc, _, MOC, _, [{Attribute, Fields}]} ->
	    %% ct:print("Received: ~n~p~n",[E]),
	    case proplists:get_value(actionId, Fields) of
		[[OldActionId]] ->
		    ct:pal("Waiting for updated progress ~s~n~p~n",
			   [OldActionId, Fields]),
		    wait_for_progress_e(Attribute, OldActionId, MOC);
		_ ->
		    case check_progress_elements_e(Fields) of
			loop ->
			    wait_for_progress_e(Attribute, OldActionId, MOC);
			{ok, Result, Fields} ->
			    {ok, Result, Fields}
		    end
	    end
    after 60000 ->
	    ct:print("Messages at timeout ~n~p~n",
		     [process_info(self(), messages)]),
	    ct:fail(event_timeout)
    end.

check_progress_elements_e([]) -> loop;
check_progress_elements_e(Fields) -> 
    [[State]] = proplists:get_value(state, Fields),
    case State of
	"FINISHED" ->
	    ct:pal(format_fields(Fields), []),
	    [Result] = proplists:get_value(result, Fields),
	    {ok, Result, Fields};
	"CANCELLED" ->
	    ct:pal(format_fields(Fields),[]),
	    [Result] = proplists:get_value(result, Fields),
	    {ok, Result, Fields};
	Current ->
	    [[Percent]] = proplists:get_value(progressPercentage, Fields),
	    [[Info]] = proplists:get_value(progressInfo, Fields),
	    [[ActionName]] = proplists:get_value(actionName, Fields),
	    ct:pal("Action: ~s~nState: ~s ~s % ready~n~s",
		   [ActionName, Current, Percent, Info]),
	    loop
    end.

format_fields(Fields) ->
    [case length(Values) of
	 1 ->
	     [io_lib:format("~w: ~s~n",[Field, hd(Values)])];
	 _ ->
	     [io_lib:format("~w:~n",[Field])|
	      [io_lib:format("  ~s~n",[hd(Value)])||Value<-Values]]
     end||{Field, Values}<-Fields].

    
    


%% @hidden
%% This is for the new event driven TCs
get_buid(ResultInfo) ->
    lists:last(string:tokens(ResultInfo, "=")).

%% @hidden
%%% Legacy case
get_buid(ProgressFilter, ResultInfo) ->
    case get_action_name(ProgressFilter) of
        "CREATE" -> BuId = lists:last(string:tokens(ResultInfo, "=")),
                    BuId;
        "MANUAL_EXPORT" -> AdditionalInfo = get_additional_info(ProgressFilter),
                           {match, BuId} = re:run(AdditionalInfo, "[0-9]+", [{capture,first,list}]),
                           BuId
    end.
    

%%--------------------------------------------------------------------
%% @doc NodeUC443.N Delete backup
%% Using netconf to trig a backup to be removed
%% Initiate the action and wait for it to be deleted. Create a backup
%% if no one exists.
%% @end

delete_backup(Config) ->
    MeId = proplists:get_value(meId, Config),
    %% Backups = get_all_backups(MeId),
    Name =
    case proplists:get_value(force_backup_name, Config, false) of
        true ->
        proplists:get_value(backupName, Config);
        false ->
        BName = "swm_backup_SUITE.delete_backup."++ftpes_test_lib:unique_name(),
        NewConfig =
            lists:keyreplace(backupName, 1, Config,
                     {backupName, BName}),
        {ok, _} = create_backup(NewConfig),
        BName

        %% case Backups of
        %%     [] ->
        %%  create_backup(Config),
        %%  [Element|_] = get_all_backups(MeId),
        %%  {'BrmBackup', _, BrmBackup} = Element,
        %%  {value, {backupName, _, BName}} =
        %%      lists:keysearch(backupName, 1, BrmBackup),
        %%  BName;
        %%     BrmBackupEs ->
        %%  Nth = rand:uniform(length(BrmBackupEs)),
        %%  BrmBackupE = lists:nth(Nth, BrmBackupEs),
        %%  {'BrmBackup', _, BrmBackup} = BrmBackupE,
        %%  {value, {backupName, _, BName}} =
        %%      lists:keysearch(backupName, 1, BrmBackup),
        %%  BName
        %% end
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
             {'deleteBackup',[],
              [{name, [], [Name]}]}]}]}]}]},
    ct:pal("Executing action deleteBackup ~s~n",[Name]),
    ok = swmTestLib:wait_for_action_capable(60000*6),
    swmTestLib:flush_mnesia_event_messages(),
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
    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
    ["SUCCESS"] ->
        ct:pal("Backup deleted successfully"),
        %% if called from clean backups, don't check
        case proplists:get_value(force_backup_name, Config, false) of
        false ->
            ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("deleteBackup")},
                                   {"CAPABLE", ""}]),
            ok = swmTestLib:wait_for_action_capable();
        _ ->
            ok
        end;
    Result ->
        case proplists:get_value(forced, Config) of
        true ->
            ct:pal("deleteBackup: ~s~n",[Result]);
        false ->
            ct:pal("deleteBackup: ~s~n",[Result]),
            ct:fail(Result)
        end
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC443.E1 Delete backup Specified name not found
%% Using netconf to trig a backup to be removed but where the name does not
%% exist
%% @end

delete_backup_not_found(Config) ->
    Name = "swm_backup_SUITE."++ftpes_test_lib:unique_name(),
    NewConfig =
    [{force_backup_name,true}|
     lists:keyreplace(backupName, 2, Config, {backupName, Name})],
    try delete_backup(NewConfig) of
    ok ->
        ct:pal("Delete backup returned ok although the backup did not exist"),
        ct:fail(delete_backup_not_found_fail)
    catch exit:{test_case_failed, backupNotFound} ->
        timer:sleep(1000),
        ok = swmTestLib:wait_for_action_capable(),
        ok
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
     NewConfig = lists:keyreplace(backupName,1, Config,
                      {backupName, Name}),
     delete_backup([{forced, true},
            {force_backup_name, true}|NewConfig])
     end||Backup<-Backups],
    ok.

%%--------------------------------------------------------------------
%% @doc NodeUC446.N Export backup
%% Using netconf to trig a backup to be exported
%% Initiate the action, and then check the progress report for completion
%% Access the backup on the export server, and verify the backup metadata
%% @end

export_backup(Config) ->
    MeId = proplists:get_value(meId, Config),
    %% Backups = get_all_backups(MeId),

    Name = "swm_backup_SUITE.export_backup."++ftpes_test_lib:unique_name(),
    NewConfig =
    lists:keyreplace(backupName, 1, Config,
             {backupName, Name}),
    {ok, ResultInfo} = create_backup(NewConfig),
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
    ActionId = get_action_id(ProgressFilter),

    Uri = ?config(uri, Config),
    Password = ?config(password, Config),
    Cancel = proplists:get_value(cancel, Config, false),
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
    RootDir = proplists:get_value(ftp_root, Config),
    %% check_server_conditions(Uri, Password),
    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),
    Ares = netconf(action, [nc1, Action]),
    case Ares of
    {ok, _} -> ok;
    Error ->
        ct:pal("backupExport action failed: ~p~n",[Error]),
        ct:fail(Error)
    end,
    if Cancel ->
           CancelAction = {'ManagedElement',
          [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
          [{managedElementId,[],[MeId]},
           {'SystemFunctions',
        [{systemFunctionsId,[],["1"]},
         {'BrM',
          [{brMId,[],["1"]},
           {'BrmBackupManager',
            [{brmBackupManagerId,[],["1"]},
             {'BrmBackup',
              [{brmBackupId, [], [Index]}, 
               {cancelCurrentAction, []}]}]}]}]}]},
           ActionRes = netconf(action, [nc1, CancelAction]),
           case ActionRes of
                {ok, _} ->  ok;
                _ -> ct:fail(ActionRes)
           end,
           Result = wait_for_progress(progressReport, ActionId, ProgressFilter),
           case get_state(ProgressFilter) of
                "CANCELLED" -> ok;
                Other -> ct:fail(Other)
           end,
           %%part of backup got exported, deleting it on client side
           {ok, RawDirData} =rct_ftpes_client:list_dir(?config(log_path, Config) ++ "/" ++ ?config(folder, Config)),
           Files = [filename:basename(string:strip(File)) || File <- string:tokens(RawDirData, "\r\n")],
           [DeleteFile|_] = [File || File <- Files, lists:prefix("FTPES_Backup_" ++ Name, File)],
           ok = rct_ftpes_client:delete_file(?config(log_path, Config) ++ "/" ++ ?config(folder, Config) ++ "/" ++ DeleteFile),
           ct:fail(Result);
       true ->
           case wait_for_progress(progressReport, ActionId, ProgressFilter) of
            ["SUCCESS"] -> {ok, Files} = file:list_dir(RootDir),
                           ct:pal("RootDir = ~p ~nFiles = ~p~n",[RootDir, Files]),
                           Res = get_result_info(ProgressFilter),
                           ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("exportBackup")}, {"CAPABLE", ""}]),
                           ok = swmTestLib:wait_for_action_capable(),
                           ExportConfig = [{delete,Res} | Config],
                           {save_config, ExportConfig};
            Result -> ct:pal("exportBackup: ~s~n",[Result]),
                      ct:pal("URI: ~s~n",[Uri]),
                      cmd(["ls -la ",RootDir]),
                      ct:fail(Result)
           end
    end.

%%--------------------------------------------------------------------
%% @doc Export backup with a slash
%% Export a backup containing the slash character ("/") in the backup name
%% @end

export_backup_slash_name(Config) ->
    MeId = proplists:get_value(meId, Config),
    Name = "swm_backup_SUITE_slash/slash."++ftpes_test_lib:unique_name(),
    NewConfig = lists:keyreplace(backupName,1, Config,
                 {backupName, Name}),
    create_backup(NewConfig),
    Backups = get_all_backups(MeId),
    [Index] =
    [begin
         {value, {brmBackupId, _, Id}} =
         lists:keysearch(brmBackupId, 1, BrmBackup),
         Id
     end||
        {'BrmBackup', _, BrmBackup}<-Backups,
        begin
        {value, {backupName, _, [BackupName]}} =
            lists:keysearch(backupName, 1, BrmBackup),
        BackupName == Name
        end],
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
    ActionId = get_action_id(ProgressFilter),
    ct:pal("ActionId = ~p~n",[ActionId]),

    Uri = ?config(uri, Config),
    Password = ?config(password, Config),
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
    RootDir = proplists:get_value(ftp_root, Config),
    %% check_server_conditions(Uri, Password),
    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),
    Ares = netconf(action, [nc1, Action]),
    case Ares of
    {ok, _} -> ok;
    Error ->
        ct:pal("backupExport action failed: ~p~n",[Error]),
        ct:fail(Error)
    end,
    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
    ["SUCCESS"] ->
        {ok, Files} = file:list_dir(RootDir),
        ct:pal("RootDir = ~p ~nFiles = ~p~n",[RootDir, Files]),
        Res = get_result_info(ProgressFilter),
        ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("exportBackup")}, {"CAPABLE", ""}]),
        ok = swmTestLib:wait_for_action_capable(),
        ok = rct_ftpes_client:delete_file(Res);
    Result ->
        ct:pal("exportBackup: ~s~n",[Result]),
        ct:pal("URI: ~s~n",[Uri]),
        cmd(["ls -la ",RootDir]),
        ct:fail(Result)
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC446.E1 Export failure
%% Using netconf to trig a backup to be exported
%% Initiate the action, and then check the progress report for completion
%% Access the backup on the export server, and verify the backup metadata
%% @end

export_failure_pass(Config) ->
    NewConfig = lists:keyreplace(password, 1, Config, {password, "wrongpassword"}),
    try export_backup(NewConfig) of
    ok ->
        ct:pal("The backup file transfer succeeded although it shouldn't"),
        ct:fail(export_failure_test_failed)
    catch exit:{test_case_failed, ["FAILURE"]} ->
        ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("exportBackup")},
                               {"CAPABLE", ""}]),
        ok = swmTestLib:wait_for_action_capable(),
        ok
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC446.E1 Export failure
%% Using netconf to trig a backup to be exported
%% Initiate the action, and then check the progress report for completion
%% Access the backup on the export server, and verify the backup metadata
%% @end

export_failure_uri(Config) ->
    Uri = ?config(uri, Config),
    NewConfig = lists:keyreplace(uri, 1, Config, {uri, Uri ++ "/WrongComPath"}),
    try export_backup(NewConfig) of
    ok ->
        ct:pal("The backup file transfer succeeded although it shouldn't"),
        ct:fail(export_failure_test_failed)
    catch exit:{test_case_failed, ["FAILURE"]} ->
        ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("exportBackup")},
                               {"CAPABLE", ""}]),
        ok = swmTestLib:wait_for_action_capable(),
        ok
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC446.E1 Export cancel
%% Using netconf to trig a backup to be exported
%% Initiate the action, and then check the progress report for completion
%% Access the backup on the export server, and verify the backup metadata
%% @end

export_cancel(Config) ->
    try export_backup([{cancel, true} | Config]) of
    ok ->
        ct:pal("The backup file transfer succeeded although it shouldn't"),
        ct:fail(export_failure_test_failed)
    catch exit:{test_case_failed, ["FAILURE"]} ->
        ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("exportBackup")},
                               {"CAPABLE", ""}]),
        ok = swmTestLib:wait_for_action_capable(),
        ok
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC446.N Automatic export backup
%% Using netconf to trig a backup to be exported
%% Initiate the action, and then check the progress report for completion
%% Access the backup on the export server, and verify the backup metadata
%% @end

autoexport_backup(Config) ->
    MeId = proplists:get_value(meId, Config),
    %% Backups = get_all_backups(MeId),
    Name = "swm_backup_SUITE.autoexport_backup."++ftpes_test_lib:unique_name(),
    NewConfig =
    lists:keyreplace(backupName, 1, Config,
             {backupName, Name}),
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
    ct:pal("ActionId = ~p~n",[ActionId]),

    Uri = ?config(uri, Config),
    Password = ?config(password, Config),
    Cancel = proplists:get_value(cancel, Config, false),
    Action = {'ManagedElement',  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
                                 [{managedElementId,[],[MeId]},
               {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                 {'BrM', [{brMId,[],["1"]},
                   {'BrmBackupManager', [{brmBackupManagerId,[],["1"]},
                                         {autoExport, ["ENABLED"]},
                                                             {autoExportPassword, [], [{cleartext, [], []}, {password, [], [Password]}]},
                                                             {autoExportUri, [], [Uri]}]}]}]}]},
    Ares = netconf(edit_config, [nc1, running, Action]),
    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),
    case Ares of
    ok -> ok;
    Error ->
        ct:pal("backupExport action failed: ~p~n",[Error]),
        ct:fail(Error)
    end,
    {ok, ResultInfo} = create_backup_e(NewConfig),
    Index = [lists:last(string:tokens(ResultInfo, "="))],
    ct:pal("Selected backup ~p for export.~n",[Index]),
    if Cancel ->
           backup_cancel(MeId),
           ct:fail(["FAILURE"]);
       true ->
	    %% case wait_for_progress(progressReport, ActionId, ProgressFilter) of
	    case wait_for_progress_e(progressReport, ActionId, "BrmBackup") of
		{ok, ["SUCCESS"], Fields} -> 
		    ct:print("Unused messages ~p~n",[process_info(self(), messages)]),
            ok = swmTestLib:wait_for_action_capable(),
            FilePath = proplists:get_value(resultInfo, Fields),
            ct:pal("URI: ~s~n", [FilePath]),
            ok = rct_ftpes_client:delete_file(FilePath),
		    ok;
		{ok, Result, _} -> 
		    ct:pal("exportBackup: ~s~n",[Result]),
                      ct:pal("URI: ~s~n",[Uri]),
                      ct:fail(Result)
           end
    end.


%%--------------------------------------------------------------------
%% @doc NodeUC446.E1 AutoExport failure
%% Using netconf to trig a backup to be autoexported
%% Initiate the action, and then check the progress report for completion
%% Access the backup on the export server, and verify the backup metadata
%% @end

autoexport_failure_pass(Config) ->
    NewConfig = lists:keyreplace(password, 1, Config, {password, "wrongpassword"}),
    try autoexport_backup(NewConfig) of
    ok ->
        ct:pal("The autoexport backup file transfer succeeded although it shouldn't"),
        ct:fail(export_failure_test_failed)
    catch exit:{test_case_failed, ["FAILURE"]} ->
        %%ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("exportBackup")},{"CAPABLE", ""}]),
        ok = swmTestLib:wait_for_action_capable(),
        ok
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC446.E1 AutoExport failure
%% Using netconf to trig a backup to be autoexported
%% Initiate the action, and then check the progress report for completion
%% Access the backup on the export server, and verify the backup metadata
%% @end

autoexport_failure_uri(Config) ->
    Uri = ?config(uri, Config),
    NewConfig = lists:keyreplace(uri, 1, Config, {uri, Uri ++ "/WrongComPath"}),
    try autoexport_backup(NewConfig) of
    ok ->
        ct:pal("The autoexport backup file transfer succeeded although it shouldn't"),
        ct:fail(export_failure_test_failed)
    catch exit:{test_case_failed, ["FAILURE"]} ->
        %%ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("exportBackup")},{"CAPABLE", ""}]),
        ok = swmTestLib:wait_for_action_capable(),
        ok
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC447.N Import Backup
%% Using netconf to cause the system to import a backup
%% Initiate the action and wait for it to be complete
%% Create and export a backup if none is available
%% @end

import_backup(Config) ->
    Uri = atom_to_list(ftpes) ++ "://"++?config(username, Config)++"@"++?config(host, Config)++?config(delete, Config),
    MeId = proplists:get_value(meId, Config),
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
    Password = ?config(password, Config),
    Cancel = proplists:get_value(cancel, Config, false),
    Action = {'ManagedElement',
          [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
          [{managedElementId,[],[MeId]},
           {'SystemFunctions',
        [{systemFunctionsId,[],["1"]},
         {'BrM',
          [{brMId,[],["1"]},
           {'BrmBackupManager',
            [{brmBackupManagerId,[],["1"]},
             {'importBackup',[],
              [{uri, [], [Uri]},
               {password, [], [Password]}]}]}]}]}]},
    ct:pal("Executing action importBackup"),
    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),
    {ok, _A} = netconf(action, [nc1, Action]),
    if Cancel ->
           backup_cancel(MeId),
           ct:fail(["FAILURE"]);
       true ->
           case wait_for_progress(progressReport, ActionId, ProgressFilter) of
            ["SUCCESS"] -> ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("importBackup")}, {"CAPABLE", ""}]),
                           ok = swmTestLib:wait_for_action_capable();
                           %%ok = rct_ftpes_client:delete_file(?config(log_path, Config) ++"/"++ImportName);
            Result -> ct:pal("importBackup: ~s~n",[Result]),
                      ct:fail(Result)
           end
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC447.E1 Import failure
%% Using netconf to trig an import of a backup but modify password to
%% make the import fail. Make sure the system reponds correctly
%% @end

import_failure_pass(Config) ->
    Uri = atom_to_list(ftpes) ++ "://"++?config(username, Config)++"@"++?config(host, Config)++?config(delete, Config),
    MeId = proplists:get_value(meId, Config),
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
    Password = "wrongpassword",
    Action = {'ManagedElement',
          [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
          [{managedElementId,[],[MeId]},
           {'SystemFunctions',
        [{systemFunctionsId,[],["1"]},
         {'BrM',
          [{brMId,[],["1"]},
           {'BrmBackupManager',
            [{brmBackupManagerId,[],["1"]},
             {'importBackup',[],
              [{uri, [], [Uri]},
               {password, [], [Password]}]}]}]}]}]},
    ct:pal("Executing action importBackup"),
    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),
    {ok, _A} = netconf(action, [nc1, Action]),
    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
        ["SUCCESS"] -> ct:pal("The backup file transfer succeeded although it shouldn't"),
                       ct:fail(import_failure_test_failed);
        _ -> timer:sleep(1000),
             ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("importBackup")},
                      {"CAPABLE", ""}]),
             ok = swmTestLib:wait_for_action_capable()
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC447.E1 Import failure
%% Using netconf to trig an import of a backup but modify URI to
%% make the import fail. Make sure the system reponds correctly
%% @end

import_failure_uri(Config) ->
    Uri = atom_to_list(ftpes) ++ "://"++?config(username, Config)++"@"++?config(host, Config)++?config(delete, Config),
    MeId = proplists:get_value(meId, Config),
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
    Password = ?config(password, Config),
    Action = {'ManagedElement',
          [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
          [{managedElementId,[],[MeId]},
           {'SystemFunctions',
        [{systemFunctionsId,[],["1"]},
         {'BrM',
          [{brMId,[],["1"]},
           {'BrmBackupManager',
            [{brmBackupManagerId,[],["1"]},
             {'importBackup',[],
              [{uri, [], [Uri ++ "Wrong"]},
               {password, [], [Password]}]}]}]}]}]},
    ct:pal("Executing action importBackup"),
    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),
    {ok, _A} = netconf(action, [nc1, Action]),
    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
     ["SUCCESS"] -> ct:pal("The backup file transfer succeeded although it shouldn't"),
                    ct:fail(import_failure_test_failed);
     _ -> timer:sleep(1000),
               ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("importBackup")},
                        {"CAPABLE", ""}]),
               ok = swmTestLib:wait_for_action_capable()
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC447.E1 Import cancel
%% Using netconf to trig an import of a backup but modify parameters to
%% make the import fail. Make sure the system reponds correctly
%% @end

import_backup_cancel(Config) ->
    Uri = atom_to_list(ftpes) ++ "://"++?config(username, Config)++"@"++?config(host, Config)++?config(delete, Config),
    MeId = proplists:get_value(meId, Config),
    Password = ?config(password, Config),
    Action = {'ManagedElement',
          [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
          [{managedElementId,[],[MeId]},
           {'SystemFunctions',
        [{systemFunctionsId,[],["1"]},
         {'BrM',
          [{brMId,[],["1"]},
           {'BrmBackupManager',
            [{brmBackupManagerId,[],["1"]},
             {'importBackup',[],
              [{uri, [], [Uri]},
               {password, [], [Password]}]}]}]}]}]},
    ct:pal("Executing action importBackup"),
    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),
    {ok, _A} = netconf(action, [nc1, Action]),
    backup_cancel(MeId),
    timer:sleep(1000),
    ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("importBackup")}, {"CAPABLE", ""}]),
    ok = swmTestLib:wait_for_action_capable().

%%--------------------------------------------------------------------
%% @doc Restore backup prior to upgrade
%% Make sure that it is possibe to restore to the backup that was made prior to backup
%% @end
restore_backup_before_upgrade(_Config) ->
    ok.


%%--------------------------------------------------------------------
%% @doc Auto export
%% Configure a single event to occur some time from now and enable auto export
%% Start with a locked scheduler
%% @end


scheduled_autoexport(Config) ->
    case do_autoexport(Config) of
    ["SUCCESS"] ->
        ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("SCHEDULED_BACKUP")},
                                                       {"WAIT", swmTestLib:map_current_action("SCHEDULED_EXPORT")},
                                                           {"CAPABLE", ""}]),
        ok = swmTestLib:wait_for_action_capable(),
        ok;
    Result ->
        ct:pal("SCHEDULED_EXPORT: ~s~n",[Result]),
        ct:fail(Result)
    end.

%%--------------------------------------------------------------------
%% @doc Auto export fail
%% Try to use auto export with a faulty password, check for the alarm to 
%% be sent out. Then try auto export with correct password and see that the
%% alarm goes a way
%% @end

scheduled_autoexport_failure_pass(Config) ->
    MeId = proplists:get_value(meId, Config),

    case is_alarm(MeId, "9175045") of
    true ->
        ct:fail("AutoExportBackupFailed alarm is already present");
    false ->
        ok
    end,

    Pass = ?config(password, Config),
    NewConfig = lists:keyreplace(password, 1, Config, {password, Pass ++ "1"}),
    case do_autoexport(NewConfig) of
    ["FAILURE"] ->
        timer:sleep(5000),
        case is_alarm(MeId, "9175045") of
        true ->
            ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("SCHEDULED_BACKUP")},
                                                       {"WAIT", swmTestLib:map_current_action("SCHEDULED_EXPORT")},
                                                           {"CAPABLE", ""}]),
            ok = swmTestLib:wait_for_action_capable(),
            ok;
        false ->
            ct:fail("AutoExportBackupFailed alarm is not present")
        end,
        case do_autoexport(Config) of
        ["SUCCESS"] -> 
            timer:sleep(5000),
            case is_alarm(MeId, "9175045") of
            true ->
                ct:fail("AutoExportBackupFailed alarm not cleared");
            false ->
                ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("SCHEDULED_BACKUP")},
                                                       {"WAIT", swmTestLib:map_current_action("SCHEDULED_EXPORT")},
                                                           {"CAPABLE", ""}]),
                ok = swmTestLib:wait_for_action_capable(),
                ok
            end,
            ok;
        Result2 ->
            ct:pal("SCHEDULED_EXPORT: ~s~n",[Result2]),
            ct:fail(Result2)
        end;
    [Result] ->
        ct:pal("Result : ~p~n",[Result]),
        ct:fail({unexpected_result, Result})
    end.

%%--------------------------------------------------------------------
%% @doc Auto export uri fail
%% Try to use auto export with a faulty URI, check for the alarm to 
%% be sent out. Then try auto export with correct URI and see that the
%% alarm goes away
%% @end

scheduled_autoexport_failure_uri(Config) ->
    MeId = proplists:get_value(meId, Config),

    case is_alarm(MeId, "9175045") of
    true ->
        ct:fail("AutoExportBackupFailed alarm is already present");
    false ->
        ok
    end,

    Uri = ?config(uri, Config),
    NewConfig = lists:keyreplace(uri, 1, Config, {uri, Uri ++ "/WrongComPath"}),
    case do_autoexport(NewConfig) of
    ["FAILURE"] ->
        timer:sleep(5000),
        case is_alarm(MeId, "9175045") of
        true ->
            ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("SCHEDULED_BACKUP")},
                                                       {"WAIT", swmTestLib:map_current_action("SCHEDULED_EXPORT")},
                                                           {"CAPABLE", ""}]),
            ok = swmTestLib:wait_for_action_capable(),
            ok;
        false ->
            ct:fail("AutoExportBackupFailed alarm is not present")
        end,
        case do_autoexport(Config) of
        ["SUCCESS"] -> 
            timer:sleep(20000),
            case is_alarm(MeId, "9175045") of
            true ->
                ct:fail("AutoExportBackupFailed alarm not cleared");
            false ->
                ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("SCHEDULED_BACKUP")},
                                                       {"WAIT", swmTestLib:map_current_action("SCHEDULED_EXPORT")},
                                                           {"CAPABLE", ""}]),
                ok = swmTestLib:wait_for_action_capable(),
                ok
            end,
            ok;
        Result2 ->
            ct:pal("SCHEDULED_EXPORT: ~s~n",[Result2]),
            ct:fail(Result2)
        end;
    [Result] ->
        ct:pal("Result : ~p~n",[Result]),
        ct:fail({unexpected_result, Result})
    end.

clear_schedule(Config) ->
    MeId = proplists:get_value(meId, Config),
    Get = {'ManagedElement',
       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
       [{managedElementId,[],[MeId]},
        {'SystemFunctions',
         [{systemFunctionsId,[],["1"]},
          {'BrM',
           [{brMId,[],["1"]},
        {'BrmBackupManager',
         [{brmBackupManagerId,[],["1"]},
          {'BrmBackupScheduler',
           [{brmBackupSchedulerId,[], ["1"]}]}]}]}]}]},
    {ok, Res} = netconf(get_config, [nc1, running, Get]),
    {ok, {'BrmBackupScheduler', Attrs, Content}} =
    extract_element('BrmBackupScheduler', Res),
    Delete = {'ManagedElement',
          [{xmlns,"urn:com:ericsson:ecim:ComTop"},
           {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
          [{managedElementId,[],[MeId]},
           {'SystemFunctions',
        [{systemFunctionsId,[],["1"]},
         {'BrM',
          [{brMId,[],["1"]},
           {'BrmBackupManager',
            [{brmBackupManagerId,[],["1"]},
             {'BrmBackupScheduler', Attrs,
              [{brmBackupSchedulerId,[], ["1"]}|
               [case element(1, Item) of
                'BrmSingleEvent' = Element ->
                {ok, Id} = extract_element(
                         brmSingleEventId, [Item]),
                {Element, [{'xc:operation', "delete"}], [Id]};
                'BrmPeriodicEvent' = Element->
                {ok, Id} = extract_element(
                         brmPeriodicEventId, [Item]),
                {Element, [{'xc:operation', "delete"}], [Id]};
                'BrmCalendarBasedPeriodicEvent' = Element ->
                {ok, Id} = extract_element(
                         brmCalendarBasedPeriodicEventId, [Item]),
                {Element, [{'xc:operation', "delete"}], [Id]}
            end||Item<-Content,
                 (element(1,Item) == 'BrmSingleEvent') or
                 (element(1, Item) == 'BrmPeriodicEvent') or
                 (element(1, Item) == 'BrmCalendarBasedPeriodicEvent')]
               ]}]}]}]}]},

    ok = netconf(edit_config, [nc1, running, Delete]).

%%%--------------------------------------------------------------------
%%% LIBRARY FUNCTIONS
%%%--------------------------------------------------------------------

%%%--------------------------------------------------------------------
%%% Description: Update manual housekeeping MO
%%%--------------------------------------------------------------------

set_housekeeping(MeId, MaxNumberOfBackups, Autodelete) ->
    Set =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[MeId]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'BrM',
             [{brMId,[],["1"]},
              {'BrmBackupManager',
               [{brmBackupManagerId,[],["1"]},
        {'BrmBackupHousekeeping',
         [{brmBackupHousekeepingId, [], ["1"]},
          {maxStoredManualBackups, [],
           [integer_to_list(MaxNumberOfBackups)]} ,
          {autoDelete, [], [Autodelete]}
         ]}]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Set]).

set_housekeeping_default(MeId) ->
    ct:print("Default housekeeping~n"),
    set_housekeeping(MeId, 5, "ENABLED").


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
%%% Description: Read the action id of the last progress report to prevent
%%%              reading old reports
%%%--------------------------------------------------------------------

get_action_id(ProgressFilter) ->
    get_progress_report_member(actionId, ProgressFilter).

%%%--------------------------------------------------------------------
%%% Description: Read the resultinfo info of the last progress report
%%%--------------------------------------------------------------------

get_result_info(ProgressFilter) ->
    get_progress_report_member(resultInfo, ProgressFilter).

%%%--------------------------------------------------------------------
%%% Description: Read the state of the last progress report
%%%--------------------------------------------------------------------

get_state(ProgressFilter) ->
    get_progress_report_member(state, ProgressFilter).

%%%--------------------------------------------------------------------
%%% Description: Read the additionalinfo info of the last progress report
%%%--------------------------------------------------------------------

get_additional_info(ProgressFilter) ->
    get_progress_report_member(additionalInfo, ProgressFilter).

%%%--------------------------------------------------------------------
%%% Description: Read the actionName of the last progress report
%%%--------------------------------------------------------------------

get_action_name(ProgressFilter) ->
    get_progress_report_member(actionName, ProgressFilter).


%%%--------------------------------------------------------------------
%%% Description: Read a member of the progress report struct
%%%--------------------------------------------------------------------

get_progress_report_member(Member, ProgressFilter) ->
    {ok, A} = netconf(get, [nc1, ProgressFilter]),
                        %    ct:pal("ProgressFilter result: ~p~n", [A]),
    case extract_element(progressReport, A) of
    {ok, {progressReport, L, _}}  ->
        case lists:keyfind(unset,1, L) of
        {unset, "true"} ->
            undefined;
        _ ->
            extract_member(Member, A)
        end;
    _ ->
        extract_member(Member, A)
    end.
extract_member(Member, A) ->
    {ok, {Member, _, [Value]}} = 
    extract_element(Member, A),
    Value.

do_autoexport(Config) ->
    MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config)++"_sct",

    ProgressFilter = {'ManagedElement',
              [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
              [{managedElementId,[],[MeId]},
               {'SystemFunctions',
            [{systemFunctionsId,[],["1"]},
             {'BrM',
              [{brMId,[],["1"]},
               {'BrmBackupManager',
                [{brmBackupManagerId,[],["1"]},
                 {'BrmBackupScheduler',
                  [{brmBackupSchedulerId, [], ["1"]},
                   {progressReport, []}]}]}]}]}]},
    ActionId = get_action_id(ProgressFilter),

    ct:pal("Previous ActionId is ~p~n",[ActionId]),

    TS = timestamp(),
    FutureTime = add_time(add_time(), TS),
    LegalTime = iso_time(FutureTime, extended),

    Uri = ?config(uri, Config),
    Password = ?config(password, Config),
    Set =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[MeId]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'BrM',
             [{brMId,[],["1"]},
              {'BrmBackupManager',
               [{brmBackupManagerId,[],["1"]},
            {'BrmBackupScheduler',
             [{brmBackupSchedulerId,[], ["1"]},
              {scheduledBackupName, [Name]},
          {autoExport, ["ENABLED"]},
          {autoExportPassword, [],
           [{cleartext, [], []},
            {password, [], [Password]}]},
          {autoExportUri, [], [Uri]},
              {'BrmSingleEvent', [],
               [{brmSingleEventId, [Name++"_sct"]},
                {scheduledTime,[LegalTime]}]}]}]}]}]}]},
    ct:pal("Scheduled time is ~p~n",[LegalTime]),

    %% This will cause the system to wait 60 seconds for a go ahead message
    %% before it resets the progress report
    ok = rct_rpc:call(rpc_1, swmLib, set_variable, 
              [backup_test_sched_bu_wait, true], 10000),

    ok = netconf(edit_config, [nc1, running, Set]),
    set_admin_state(Config, "UNLOCKED"),
    timer:sleep(3000),
    Announced = get_next_scheduled_time(MeId),
    ct:pal("Announced time is ~p~n", [Announced]),

    case is_alarm(MeId, "9175044") of
    true ->
        ct:fail("ScheduledBackupFailed alarm is present");
    false ->
        ok
    end,
    
    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),
    
    BackupDn = 
    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
        ["SUCCESS"] ->
        Res = get_result_info(ProgressFilter),
        ct:pal("resultInfo: ~p~n",[Res]),
        Res;
        Result ->
        ct:pal("schedule Backup: ~s~n",[Result]),
        ct:fail(Result)
    end,
    rct_rpc:call(rpc_1, erlang, send, [swmBackupScheduler, go_ahead], 10000),
    Index = lists:last(string:tokens(BackupDn, "=,")),
    ProgressFilter2 = {'ManagedElement',
               [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
               [{managedElementId,[],[MeId]},
            {'SystemFunctions',
             [{systemFunctionsId,[],["1"]},
              {'BrM',
               [{brMId,[],["1"]},
                {'BrmBackupManager',
                 [{brmBackupManagerId,[],["1"]},
                  {'BrmBackup',
                   [{brmBackupId, [], [Index]},
                {progressReport, []}]}]}]}]}]},
    %% This should be a newly created backup Mo so it won't have a previous
    %% ActionId
    
    case wait_for_progress(progressReport, undefined, ProgressFilter2) of
        ["SUCCESS"] -> Res2 = get_result_info(ProgressFilter2),
                       ok = rct_ftpes_client:delete_file(Res2),
                       ["SUCCESS"];
        Result2 -> Result2
    end.

is_alarm(MeId, MinorType) ->
    Res = swmSchedBuLib:is_alarm(nc1, MeId, MinorType),
    Res.

add_time() ->
    case rct_rpc:call(rpc_1, sysEnv, rcs_mode, [], 10000) of
    simulated ->
        15;
    target ->
        120
    end.

%%%--------------------------------------------------------------------
%%% Description: Set the BrmBackupScheduler.adminState attribute
%%%--------------------------------------------------------------------

set_admin_state(Config, Value) ->
    MeId = proplists:get_value(meId, Config),
    Set =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[MeId]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'BrM',
             [{brMId,[],["1"]},
              {'BrmBackupManager',
               [{brmBackupManagerId,[],["1"]},
            {'BrmBackupScheduler',
             [{brmBackupSchedulerId,[], ["1"]},
          {adminState, [Value]}]}]}]}]}]},

    ok = netconf(edit_config, [nc1, running, Set]).

%%%--------------------------------------------------------------------
%%% Description: Read the value of BrmBackupScheduler.nextScheduledTime
%%%--------------------------------------------------------------------

get_next_scheduled_time(MeId) ->
    Get = {'ManagedElement',
       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
       [{managedElementId,[],[MeId]},
        {'SystemFunctions',
         [{systemFunctionsId,[],["1"]},
          {'BrM',
           [{brMId,[],["1"]},
        {'BrmBackupManager',
         [{brmBackupManagerId,[],["1"]},
          {'BrmBackupScheduler',
           [{brmBackupSchedulerId, [], ["1"]},
            {nextScheduledTime, []}]}]}]}]}]},

    {ok, A} = netconf(get, [nc1, Get]),
    case extract_element(nextScheduledTime, A) of
    {ok, {nextScheduledTime, [{unset, "true"}], []}} ->
        undefined;
    {ok, {nextScheduledTime, _, [Time]}} ->
        Time
    end.


%%% ----------------------------------------------------------
%%% @doc Convert an os:timestamp() tuple to an ISO 8601 string
%%%
%%% Input: Now  - An os:timestamp() tuple
%%%        Type - basic|extended|extended_zonefree|extended_z
%%% Output: string()
%%% @end
%%% ----------------------------------------------------------

iso_time(Now, Type) ->
    fn_date(Now, Type)++"T"++fn_time(Now, Type).

%% time_offset(Now) ->
%%     DT = calendar:now_to_local_time(Now),
%%     UTC = calendar:now_to_universal_time(Now),
%%     DiffSecs = calendar:datetime_to_gregorian_seconds(DT) -
%%         calendar:datetime_to_gregorian_seconds(UTC),
%%     [Sign, DH, DM] = diff(DiffSecs),
%%     lists:append([[Sign], padzero(DH), ":", padzero(DM)]).


fn_date(Now, Type) ->
    {{Y,M,D}, _} = calendar:now_to_local_time(Now),
    case Type of
        basic ->
            lists:append([integer_to_list(Y),
                          padzero(M),
                          padzero(D)]);
    extended_z ->
        {{YU,MU,DU}, _} = calendar:now_to_universal_time(Now),

            lists:append([integer_to_list(YU), "-",
                          padzero(MU), "-", padzero(DU)]);

        Extended when Extended==extended; Extended==extended_zonefree ->
            lists:append([integer_to_list(Y), "-",
                          padzero(M), "-", padzero(D)])
    end.

fn_time(Now, Type) ->
    DT={_, {H, M, S}} = calendar:now_to_local_time(Now),
    UTC = calendar:now_to_universal_time(Now),
    DiffSecs = calendar:datetime_to_gregorian_seconds(DT) -
        calendar:datetime_to_gregorian_seconds(UTC),
    [Sign, DH, DM] = diff(DiffSecs),
    case Type of
        basic ->
            lists:append([padzero(H),
                          padzero(M),
                          padzero(S),
                          [Sign],
                          padzero(DH),
                          padzero(DM)]);
    extended_z ->
        {_, {HU, MU, SU}} = UTC,
            lists:append([padzero(HU), ":",padzero(MU), ":",padzero(SU),"Z"]);
        extended ->
            lists:append([padzero(H), ":", padzero(M), ":", padzero(S),
                          [Sign], padzero(DH), ":", padzero(DM)]);
        extended_zonefree ->
            lists:append([padzero(H), ":", padzero(M), ":", padzero(S)])
    end.

%% format_date({{Y,M,D},{H,Mi,S}}) ->
%%     lists:append([integer_to_list(Y), "-", padzero(M), "-", padzero(D),"T",
%%        padzero(H), ":", padzero(Mi), ":", padzero(S)]).


padzero(N) ->
    if N<10 -> [$0, N+$0];
       true -> integer_to_list(N)
    end.


diff(Secs) ->
    case calendar:seconds_to_daystime(Secs) of
        {0, {H, M,_}} ->
                [$+, H, M];
        {-1, _} ->
                {0, {H, M, _}} = calendar:seconds_to_daystime(-Secs),
                [$-, H, M]
    end.

%%%--------------------------------------------------------------------
%%% Description: Adds seconds to a now-tuple
%%%--------------------------------------------------------------------

add_time(N, {N1,N2,N3}) ->
    case N2+N of
    F2 when F2 >= 1000000 ->
        {N1+F2 div 1000000, F2 rem 1000000, N3};
    F2 ->
        {N1, F2, N3}
    end.

%%%--------------------------------------------------------------------
%%% Description: Get node time
%%%--------------------------------------------------------------------

timestamp() ->
    rct_rpc:call(rpc_1, os, timestamp, [], 10000).

%%%--------------------------------------------------------------------
%%% Description: Netconf
%%%--------------------------------------------------------------------

netconf(F, A) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    case apply(ct_netconfc, F, A) of
    ok ->
        ok = ct_netconfc:close_session(nc1),
        ok;
    {ok, R} ->
        ok = ct_netconfc:close_session(nc1),
        {ok, R};
    {error, Error} ->
        {error, Error}
    end.

%%%--------------------------------------------------------------------
%%% Description: Make a standardized rpc call
%%%--------------------------------------------------------------------

call(M, F, A) ->
    rct_rpc:call(rpc_1, M, F, A, 20000).

%%%--------------------------------------------------------------------
%%% Description: This function and the associated check_progress_elements
%%%              loops over a netconf get for the progress information until
%%%              the state is FINISHED, otherwise it prints the status and
%%%              progress percentage
%%%--------------------------------------------------------------------


wait_for_progress(Attribute, OldActionId, ProgressFilter) ->
    timer:sleep(1000),
    case ct_netconfc:open(nc1, []) of
    {ok, _} ->
        Get = ct_netconfc:get(nc1, ProgressFilter),
        ct_netconfc:close_session(nc1),
        case Get of
        {ok, Report} ->
            case extract_element(actionId, Report) of
            {ok, {actionId, _, [OldActionId]}} ->
                wait_for_progress(Attribute, OldActionId,
                          ProgressFilter);
            not_found ->
                wait_for_progress(Attribute, OldActionId,
                          ProgressFilter);
            _ ->
                case check_progress_elements(Attribute, Report) of
                loop ->
                    wait_for_progress(Attribute, OldActionId,
                              ProgressFilter);
                {ok, Result} ->
                    Result
                end
            end;
        {error, Error} ->
            ct:pal("Netconf get error:~n~p",[Error]),
            wait_for_progress(Attribute, OldActionId, ProgressFilter)
        end;
    {error, Reason} ->
        ct:log("Netconf open error:~n~p",[Reason]),
        wait_for_progress(Attribute, OldActionId, ProgressFilter)
    end.


check_progress_elements(Attribute, ProgressReport) ->
    {ok, Report} = extract_element(Attribute, ProgressReport),
    case Report of
    {progressReport, [{unset, "true"}], []} ->
        loop;
    _ ->
        {ok, State} = extract_element(state, [Report]),
        case State of
        {state, _, ["FINISHED"]} ->
            format_progress_report(Report),
            ct:log("~s",[format_progress_report(Report)]),
            {ok, {result, _, Result}} =
            extract_element(result, [Report]),
            {ok, Result};
        {state, _, ["CANCELLED"]} ->
            format_progress_report(Report),
            ct:log("~s",[format_progress_report(Report)]),
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



%%%--------------------------------------------------------------------
%%% Description: Format a AsyncActionStruct for printing
%%%--------------------------------------------------------------------


format_progress_report({progressReport, _, Members}) ->
    [io_lib:format("progressReport:~n",[])|format_progress_report(Members)];
format_progress_report({progress, _, Members}) ->
    [io_lib:format("progress:~n",[])|format_progress_report(Members)];
format_progress_report([{Key, _, [Value]}|Members]) ->
    [io_lib:format("~w: ~s ~n",[Key, Value])|
     format_progress_report(Members)];
format_progress_report([{Key, _, []}|Members]) ->
    [io_lib:format("~w: ~s ~n",[Key, ""])|
     format_progress_report(Members)];
format_progress_report([]) -> [];
format_progress_report(X) ->
    ct:pal("Unknown format: ~p~n",[X]),
    ct:fail(unknown_progress_report_format).



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

backup_cancel(MeId) ->
    Conf = {'ManagedElement',
          [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
          [{managedElementId,[],[MeId]},
           {'SystemFunctions',
        [{systemFunctionsId,[],["1"]},
         {'BrM',
          [{brMId,[],["1"]},
           {'BrmBackupManager',
            [{brmBackupManagerId,[],["1"]},
         {cancelCurrentAction, [], []}]}]}]}]},

    ActionRes = netconf(action, [nc1, Conf]),
    case ActionRes of
    {ok, _} -> ok;
    _ ->
        ct:fail(ActionRes)
    end.

flush_messages() ->
    receive _ -> flush_messages()
    after 0 -> ok 
    end.
	    

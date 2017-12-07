%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_backup_SUITE.erl %
%%% @author etxpejn
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R11A/12
%%%
%%% @doc == Test Suite for testing the transfer ESI from SUT using netconf.==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% @end

-module(swm_backup_SUITE).
-vsn('/main/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R11A/12').
-author('etxpejn').
-date('2017-11-09').
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
%%% R1A/1      2012-09-20 etxjotj     Created
%%% R2A/1      2013-01-17 etxkols     Change ct:pal to ct:log since
%%%                                   Jenkins detects error in console
%%% R2A/5      2013-02-28 etxkols     Added rct_core hook
%%% R1A/6      2013-06-17 etxjovp     Add hook rct_logging
%%% R2A/18     2013-07-08 etxarnu     Corrected match to return values
%%%                                   for sysNetloader:get_active_instance
%%% R2A/19     2013-07-08 etxarnu     Reverted previous change :(
%%% R2A/44     2014-02-03 etxkols     Changed ct:pal to ct:log because of Jenkins parsing
%%% R2A/46     2014-04-08 erarafo     Deprecation warning
%%% R2A/47     2014-04-15 etxjotj     Removed deprecation warning
%%% R2A/49     2014-06-02 etxjotj     Reduced housekeeping exeuction time
%%% R2A/50     2014-07-08 etxjotj     HR77233 adjustments for backup file naming
%%% R2A/56     2014-09-12 etxjotj     Don't print "error" when it's not a fault
%%% R2A/58     2014-09-25 etxjotj     Fixed import backup problem
%%% R2A/59     2014-09-26 etxjotj     Fixed unused variable compile Warning.
%%% R2A/61     2014-10-07 etxivri     Update to move SUITE to block SWM.
%%% R3A/3      2014-12-11 etxjotj     ECIM BrM 3.3
%%% R3A/4      2015-02-18 etxivri     Update to remove error printouts in ct-shell
%%% R3A/5      2015-02-26 etxkols     Use rct_sftp hook
%%% R3A/6      2015-02-27 etxkols     Use ct:get_config instead of rct_sftp hook
%%% R3A/7      2015-02-28 etxkols     Preparation for cluster
%%% R3A/8      2015-02-28 etxkols     Preparation for cluster
%%% R3A/11     2015-03-27 etxjotj     Added TC for default backup name
%%% R3A/12     2015-04-30 etxjotj     Interrupted restore
%%% R4A/1      2015-05-07 etxarnu     Corrected get_progress_report_member
%%% R4A/2      2015-07-10 etxjovp     Add group definitions used by CS CI
%%% R4A/6      2015-11-04 etxjotj     Test for internal housekeeping
%%% R4A/9      2015-11-11 etxjotj     Make sure printouts gets where they 
%%%                                   should be
%%% R4A/10     2015-12-15 emarktu     Internal houskeeping fix (jotj?)
%%% R4A/11     2016-03-18 etxjotj     Adapt new timing for failsafe deactivate
%%% R5A/3      2016-03-29 ekurnik     Updated with actionCapable checks
%%% R5A/5      2016-04-06 ekurnik     Updated for OAM over IPv6
%%% R6A/2      2016-08-29 etxkols     Fixes for GIT
%%% R6A/4      2016-09-15 etxberb     Removed restore_backup/1. Will be replaced
%%%                                   later.
%%% R7A/1      2016-09-06 etxpejn     Changed random to rand
%%% R7A/2      2016-09-29 etxberb     Merged from R6A/6
%%% R7A/3      2016-10-17 egjknpa     add FAKE_VNFM for vrcs 
%%% R8A/1      2016-11-02 etxpejn     Changed timeout for rcp call to 20 sec
%%% R8A/2      2017-01-03 etxkols     Temporarily removed import_failure from 
%%%                                   all/0 util etxivri has looked at the testcase
%%% R8A/3      2017-01-03 etxkols     Temporarily removed import_failure also in groups
%%% R8A/4      2017-01-09 etxberb     Corrected import_failure and re-inserted
%%%                                   it in all/0 & groups/0.
%%% R8A/5      2017-01-10 etxberb     * Replaced check_action_capable("CAPABLE")
%%%                                     with wait_for_action_capable().
%%%                                   * Copied restore_backup/1 back.
%%%--------------------------------------------------------------------
%%% R9A/1   2016-02-23 enekdav  FTPES test group added
%%% R9A/3   2016-03-02 etxkols  changed ct:pal to ct:log to avoid that 
%%%                             Jenkins find error when parsing console log
%%% R9A/5   2017-03-06 etxpejn  Added new tc to be used on full node testing
%%%                             with radio.
%%% R9A/6   2017-03-08 etxpejn  Prolong timer wait_for_action_capable in 
%%%                             delete_backup
%%% R9A/9   2017-03-28 etxjotj  Don't use confirmRestore timer in this suite
%%% R9A/10  2016-03-31 enekdav  FTPES test group deleted, new suite for FTPES group is created
%%% R11A/1  2017-09-19 etxjotj  Added create_backup_e
%%% R11A/2  2017-09-20 etxjotj  Fixed duplicate name
%%% R11A/3  2017-09-20 etxjotj  Fix for SIM
%%% R11A/4  2017-09-29 etxjotj  Included create_backup_e in suite. 
%%%                             Added delete_backup_e
%%% R11A/5  2017-10-09 etxjotj  Import backup is now AVC driven
%%%                             Removed delete_backup
%%% R11A/11 2017-11-09 etxpejn  Changed create_backup to create_backup_e in nocover__group
%%%--------------------------------------------------------------------


%%compile([export_all]).
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
	 create_backup_e/1, create_backup_cancelled/1,
	 create_backup_default_name/1,
	 create_backup_name_validation_failure/1,
	 create_backup_housekeeping/1,
	 create_backup_housekeeping_autodelete/1,
	 export_backup/1,
	 export_backup_slash_name/1,
	 export_failure/1,
	 export_failure_pass/1,
	 export_failure_uri/1,
	 export_cancel/1,
	 autoexport_backup/1,
	 autoexport_failure_pass/1,
	 autoexport_failure_uri/1,
	 delete_backup/1,
	 delete_backup_e/1,
	 delete_backup_not_found/1,
	 import_backup/1,
	 import_failure_pass/1,
	 import_failure_uri/1,
	 import_backup_cancel/1,
	 clean_backups/1,
	 restore_backup/1,
	 restore_backup_interrupted/1,
	 internal_housekeeping/1,
	 restore_backup_before_upgrade/1]).

-export([tc/1]).

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [{group, legacy_group}].

legacy_group() ->
    [create_backup_e, create_backup_default_name, 
     create_backup_cancelled,
     create_backup_name_validation_failure,
     create_backup_housekeeping, create_backup_housekeeping_autodelete,
     export_backup,
     export_backup_slash_name,
     export_failure_pass,
     export_failure_uri,
     export_cancel,
     delete_backup_e, delete_backup_not_found,
     autoexport_backup,
     autoexport_failure_pass,
     autoexport_failure_uri,
     import_backup, 
     import_failure_pass,
     import_failure_uri,
     import_backup_cancel,
     internal_housekeeping,
     restore_backup
    ].

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
			    {["ERROR REPORT","CRASH REPORT"],[]}}]}}
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
				  {["ERROR REPORT","CRASH REPORT"],[]}}]}}
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
    Name = "swm_backup_SUITE",
    [{host, Host},{username, Username},{password, Password}] = 
        ct:get_config(rct_oamap_ipv:sftp_server_iptype()),
    NewConfig = [{meId, MeId},
		 {backupName, Name},
		 {sftp_host, Host},
		 {sftp_user, Username},
		 {sftp_pass, Password},
		 %% {sftp_host, "10.68.200.11"},
		 %% {sftp_user, "mauve"},
		 %% {sftp_pass, "dilbert"},
		 {sftp_root, RootDir} | Config],
    call(swmLib, set_variable, [swm_basic_tests_return_pid, self()]),
    call(swmServer, start_test_event_server, []),
    swm_event_lib:start_subscription(nc_event,[]),
    clean_backups(NewConfig),
    call(swmServer, stop_test_event_server, []),
    swm_event_lib:stop_subscription(),
    ct:print("Init per suite complete~n"),
    %% fake VNFM for vrcs
    case call(sysEnv, rcs_mode_2, []) of
        vrcs ->
            call(os, putenv, ["FAKE_VNFM", ""]);
        _ ->
            call(os, unsetenv, ["FAKE_VNFM"])
    end,
    NewConfig.

%%--------------------------------------------------------------------
%% @hidden
end_per_suite(_Config) ->
    %% fake VNFM for vrcs
    case call(sysEnv, rcs_mode_2, []) of
        vrcs ->
            call(os, unsetenv, ["FAKE_VNFM"]);
        _ ->
            ok
    end,
    ok.

%%--------------------------------------------------------------------
%% @hidden
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @hidden
init_per_testcase(create_backup_cancelled = TestCase, Config) ->
    NewConfig = init_per_testcase_default(TestCase, Config),
    call(swmLib, set_variable, [create_backup_cancel_timeout, 300000]),
    NewConfig;
init_per_testcase(import_failure = TestCase, Config) ->
    NewConfig = init_per_testcase_default(TestCase, Config),
    case existing_backups(NewConfig) of
	[] ->
	    export_backup(NewConfig);
	_ ->
	    ok
    end,
    NewConfig;
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
    [{sftp_uri, sftp_uri(Config)} |Config].

%%--------------------------------------------------------------------
%% @hidden
end_per_testcase(TestCase = create_backup_cancelled, _Config) ->
    ct:print("Test case ~w complete.~n",[TestCase]),
    swm_event_lib:stop_subscription(),
    call(swmServer, stop_test_event_server, []),
    call(swmLib, erase_variable, [swm_basic_tests_return_pid]),
    ok = call(swmLib, erase_variable, [create_backup_cancel_timeout]);
end_per_testcase(TestCase, Config) when TestCase =:= autoexport_backup orelse
                                        TestCase =:= autoexport_failure_pass orelse
                                        TestCase =:= autoexport_failure_uri ->
    MeId = proplists:get_value(meId, Config),
    Uri = proplists:get_value(sftp_uri, Config),
    Password = proplists:get_value(sftp_pass, Config),
    EditAction = {'ManagedElement',  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
                                 [{managedElementId,[],[MeId]},
               {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                 {'BrM', [{brMId,[],["1"]},
                   {'BrmBackupManager', 
		    [{brmBackupManagerId,[],["1"]},
		     {autoExport, ["DISABLED"]},
		     {autoExportPassword, [], 
		      [{cleartext, [], []}, {password, [], [Password]}]},
		     {autoExportUri, [], [Uri]}]}]}]}]},
    netconf(edit_config, [nc1, running, EditAction]),
    ok = swmTestLib:wait_for_action_capable(),
    swm_event_lib:stop_subscription(),
    swmTestLib:flush_mnesia_event_messages(),
    flush_messages(),
    ok;
end_per_testcase(TestCase = restore_backup, Config) ->
    %% Restore timeout to default
    swm_event_lib:stop_subscription(),
    MeId = proplists:get_value(meId, Config),
    Set = {'ManagedElement',
	   [],
	   [{managedElementId,[],[MeId]},
	    {userLabel, [], ["TestBeforeBackup"]},
	    {'SystemFunctions',[],
	     [{systemFunctionsId, [], ["1"]},
	      {'BrM',[],
	       [{brMId, [], ["1"]},
		{'BrmRollbackAtRestore', [],
		 [{brmRollbackAtRestoreId, [], ["1"]},
		  {timeAllowedBeforeRollback,[],["3600"]}]}
	       ]}]}]},

    ok = netconf(edit_config, [nc1, running, Set]),
    ct:print("Test case ~w complete.~n",[TestCase]),
    swm_event_lib:stop_subscription(),
    call(swmServer, stop_test_event_server, []),
    call(swmLib, erase_variable, [swm_basic_tests_return_pid]),
    ok;

end_per_testcase(TestCase, _Config) ->
    ct:print("Test case ~w complete.~n",[TestCase]),
    swm_event_lib:stop_subscription(),
    call(swmServer, stop_test_event_server, []),
    call(swmLib, erase_variable, [swm_basic_tests_return_pid]),
    ok.


%%--------------------------------------------------------------------
%% @hidden
groups() ->
    LegacyGroup = legacy_group(),
    [{legacy_group, [], LegacyGroup},
    {default__group, [], LegacyGroup},
     {cover__group, [], [ create_backup_name_validation_failure,
              create_backup_housekeeping,
              create_backup_housekeeping_autodelete,
              export_backup,
              export_backup_slash_name,
              export_failure_pass,
              export_failure_uri,
              export_cancel,
              delete_backup, 
              delete_backup_not_found,
              autoexport_backup,
              autoexport_failure_pass,
              autoexport_failure_uri,
              import_backup,
              import_failure_pass,
              import_failure_uri,
              import_backup_cancel]},
     {nocover__group, [], [create_backup_e,
			   create_backup_e,
			   clean_backups,
			   restore_backup,
			   restore_backup_interrupted]},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},  
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},  
     {sdc__def__all__1__group, [], [{group, default__group}]}, 
     {sdc__cover__sim__1__group, [], [{group, cover__group}]}, 
     {sdc__nocover__sim__1__group, [], [{group, nocover__group}]},
     {sdc__qual__all__1__group, [], []} 
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
    ok = swmTestLib:wait_for_action_capable_e(),    
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
    ok = swmTestLib:wait_for_WAIT(30000),

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
		    %% ok = swmTestLib:check_action_capable_progress(
		    %% 	   [{"WAIT", 
		    %% 	     swmTestLib:map_current_action("createBackup")},
		    %% 	    {"CAPABLE", ""}]),
		    ok = swmTestLib:wait_for_action_capable_e(),
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
	    ct:print("Messages at timeout after ~w ms ~n~p~n",
		     [60000, process_info(self(), messages)]),
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
        "MANUAL_EXPORT" -> 
	    AdditionalInfo = get_additional_info(ProgressFilter),
	    {match, BuId} = 
		re:run(AdditionalInfo, "[0-9]+", [{capture,first,list}]),
	    BuId
    end.
    
%%--------------------------------------------------------------------
%% @doc NodeUC441.A1 Create backup cancellation
%% Using netconf to trig a backup to be created and cancekked
%% Initiate the action, and then check the model for the expected instance
%% @end

create_backup_cancelled(Config)->
    MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config)++"cancelled",

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
		    Error1 = decode_return_value(ReturnValue),
		    ct:pal("Return value is ~w (~s)~n",[Error1, ReturnValue]),
		    ct:fail(Error1)
	    end;
	{error, ErrorResponse} ->
	    {ok, {'error-message', _, [ErrorMessage]}} =
		extract_element('error-message', ErrorResponse),
	    Error2 = decode_error_message(ErrorMessage),
	    ct:print("Action failure~n",[]),
	    ct:log("~s~n",[ErrorMessage]),
	    ct:fail(Error2)
    end,
    Action2 = {'ManagedElement',
	       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	       [{managedElementId,[],[MeId]},
		{'SystemFunctions',
		 [{systemFunctionsId,[],["1"]},
		  {'BrM',
		   [{brMId,[],["1"]},
		    {'BrmBackupManager',
		     [{brmBackupManagerId,[],["1"]},
		      {'cancelCurrentAction',[], []}]}]}]}]},
    ct:pal("Executing action cancelCurrentAction"),
    timer:sleep(500),
    case netconf(action, [nc1, Action2]) of
	{ok, ActionResponse2} ->
	    case extract_element(returnValue, ActionResponse2) of
		{ok, {returnValue, _, ["0"]}} ->
		    ok;
		{ok, {returnValue, _, [ReturnValue2]}} ->
		    Error11 = decode_return_value(ReturnValue2),
		    ct:pal("Return value is ~w (~s)~n",[Error11, ReturnValue2]),
		    ct:fail(Error11)
	    end;
	{error, ErrorResponse2} ->
	    {ok, {'error-message', _, [ErrorMessage2]}} =
		extract_element('error-message', ErrorResponse2),
	    Error21 = decode_error_message(ErrorMessage2),
	    ct:print("Action failure~n",[]),
	    ct:log("~s~n",[ErrorMessage2]),
	    ct:fail(Error21)
    end,
    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
	["FAILURE"] ->
	    ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("createBackup")},
							   {"CAPABLE", ""}]),
	    ok = swmTestLib:wait_for_action_capable(),
	    ok;
	Result ->
	    ct:pal("createBackup: ~s~n",[Result]),
	    ct:fail(Result)
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC441.N Create backup with default name
%% Using netconf to trig a backup to be created.
%% Initiate the action, and then check the model for the expected instance
%% @end

create_backup_default_name(Config)->
    MeId = proplists:get_value(meId, Config),
						%    Name = proplists:get_value(backupName, Config),

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
		     {'createBackup',[],[]}]}]}]}]},
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
	    ct:pal("ResultInfo ~p~n",[ResultInfo]),
	    BuId = lists:last(string:tokens(ResultInfo, "=")),
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

%%--------------------------------------------------------------------
%% @doc NodeUC441.E1 Create backup with name validation failure
%% Using netconf to trig a backup to be created.
%% Look for created backups and try to make one with the same name.
%% There should at least be one as a result of the normal test case
%% @end

create_backup_name_validation_failure(Config)->
    MeId = proplists:get_value(meId, Config),
    Backups = get_all_backups(MeId),
    case extract_element(backupName, Backups) of
	{ok, {_, _, Name}} ->
	    NewConfig =
		lists:keyreplace(backupName,1, Config, {backupName, hd(Name)}),
	    try create_backup(NewConfig) of
		{ok, _} ->
		    ct:pal("The create backup succeded although there is a duplicate name~n"),
		    ct:fail(name_validation_failure)
	    catch exit:{test_case_failed, duplicateName} ->
		    timer:sleep(1000),
		    ok = swmTestLib:wait_for_action_capable(),
		    ok
	    end;
	not_found ->
	    ct:pal("Cannot find a backup, making one"),
	    create_backup(Config),
	    create_backup_name_validation_failure(Config)

    end.

%%--------------------------------------------------------------------
%% @doc NodeUC441.E2 Housekeeping required, autodelete disabled
%% Try to create more backups that what's allowed. The last create attempt
%% should be blocked.
%% @end

create_backup_housekeeping(Config) ->
    MeId = proplists:get_value(meId, Config),
    %% Backups = get_all_backups(MeId),
    MaxNumberOfBackups = rand:uniform(5)+5,
    ct:pal("MaxNumberOfBackups = ~p~n",[MaxNumberOfBackups]),
    set_housekeeping(MeId, MaxNumberOfBackups, "DISABLED"),

    create_backups(Config, MaxNumberOfBackups),

    Name = proplists:get_value(backupName, Config),
    LastNewConfig =
	lists:keyreplace(backupName, 1, Config,
			 {backupName, Name++".last."++ftpes_test_lib:unique_name()}),
    try create_backup(LastNewConfig) of
	{ok, _} ->
	    ct:pal("The create backup succeeded although the maximum number "
		   "of backups are present."),
	    ct:fail(housekeeping_limit_failed)
    catch exit:{test_case_failed, housekeepingRequired} ->
	    ok
    end,
    ok.

%%--------------------------------------------------------------------
%% @doc NodeUC441.E2 Housekeeping required, autodelete enabled
%% Try to create more backups that what's allowed. The oldest backup
%% should be deleted
%% @end

create_backup_housekeeping_autodelete(Config) ->
    MeId = proplists:get_value(meId, Config),
    %% Backups = get_all_backups(MeId),
    MaxNumberOfBackups = rand:uniform(5)+5,
    ct:pal("MaxNumberOfBackups = ~p~n",[MaxNumberOfBackups]),
    set_housekeeping(MeId, MaxNumberOfBackups, "ENABLED"),

    create_backups(Config, MaxNumberOfBackups),

    Name = proplists:get_value(backupName, Config),
    %% Then try to make one last backup. It should go ok

    LastNewConfig =
	lists:keyreplace(backupName, 1, Config,
			 {backupName, Name++".last."++ftpes_test_lib:unique_name()}),
    create_backup(LastNewConfig).

%%--------------------------------------------------------------------
%% @doc NodeUC442.N Restore backup
%% @end

restore_backup(Config) ->
    %% clean_backups(Config),
    
    MeId = proplists:get_value(meId, Config),
    Set = {'ManagedElement',
	   [{'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
	   [{managedElementId,[],[MeId]},
	    {userLabel, [], ["TestBeforeBackup"]},
	    {'SystemFunctions',[],
	     [{systemFunctionsId, [], ["1"]},
	      {'BrM',[],
	       [{brMId, [], ["1"]},
		{'BrmRollbackAtRestore', [],
		 [{brmRollbackAtRestoreId, [], ["1"]},
		  {timeAllowedBeforeRollback,[{'xc:operation', "delete"}],[]}]}
		]}]}]},

    ok = netconf(edit_config, [nc1, running, Set]),
    
    LastNewConfig =
	lists:keyreplace(backupName, 1, Config,
			 {backupName, "swm_backup_SUITE.restore."++ftpes_test_lib:unique_name()}),
    {ok, MoRef} = create_backup(LastNewConfig),
    ct:pal("Create backup is complete ~p~n",[MoRef]),
    
    Set2 = {'ManagedElement',
	    [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	    [{managedElementId,[],[MeId]},
	     {userLabel, [], ["TestAfterBackup"]}]},
    ok = netconf(edit_config, [nc1, running, Set2]),
    
    BuId = lists:last(string:tokens(MoRef, "=")),
    %% [{'BrmBackup', _, BrmBackup}|_] = get_all_backups(MeId),
    %% {value, {brmBackupId, _, Id}} =
    %%  lists:keysearch(brmBackupId, 1, BrmBackup),
    
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
		 [{brmBackupId, [], [BuId]},
		  {progressReport, []}]}]}]}]}]},
    ActionId = get_action_id(ProgressFilter),
    ct:pal("ActionId = ~p~n",[ActionId]),
    
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
		      [{brmBackupId, [], [BuId]},
		       {restore, []}]}]}]}]}]},
    ct:print("Executing action restore on backup ~p~n",[BuId]),
    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),
    {ok, ActionResponse} = netconf(action, [nc1, Action]),
    case extract_element(returnValue, ActionResponse) of
	{ok, {returnValue, _, ["0"]}} ->
	    ok;
	{ok, {returnValue, _, [ReturnValue]}} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)~n",[Error, ReturnValue]),
	    ct:fail(Error)
    end,
    
    ok = swmTestLib:check_action_capable_progress(
	   [{"WAIT", swmTestLib:map_current_action("restoreBackup")}]),
    ct:print("Disconnecting from node pending restart...~n",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    net_kernel:disconnect(ErlNode),
    case os:getenv("SIM_OR_TARGET") of
	"target" ->
	    %% Wait for node to restart, to prevent error in ct-shell.
	    case ct_telnet:expect(console,
				  "Ericsson",
				  [{idle_timeout,300000},no_prompt_check]) of
		{ok, _} -> ok;
		{error, {no_such_name,console}} ->
		    ct:print("Console is busy. Waiting 4 mins~n",[]),
		    timer:sleep(240000)
	    end;
	_ ->
	    ok
    end,
    
    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
	["SUCCESS"] ->
	    GetC = {'ManagedElement',
		    [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		    [{managedElementId,[],[MeId]},
		     {userLabel, []}]},
	    GetResult = netconf(get_config, [nc1, running, GetC]),
	    ct:pal("Check after restore: ~p~n",[GetResult]),
	    ok = swmTestLib:wait_for_action_capable();
	Result ->
	    ct:pal("restoreBackup: ~s~n",[Result]),
	    ct:fail(Result)
    end.


%%--------------------------------------------------------------------
%% @doc Restore backup interrupted
%% The restore is interrupted with a power cycle. Check that progress 
%% report is properly updated
%% @end

restore_backup_interrupted(Config) ->
    %% clean_backups(Config),

    MeId = proplists:get_value(meId, Config),

    LastNewConfig =
	lists:keyreplace(backupName, 1, Config,
			 {backupName, "swm_backup_SUITE.restoreinterrupt."++ftpes_test_lib:unique_name()}),
    {ok, MoRef} = create_backup(LastNewConfig),
    ct:pal("Create backup is complete ~p~n",[MoRef]),


    BuId = lists:last(string:tokens(MoRef, "=")),

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
		 [{brmBackupId, [], [BuId]},
		  {progressReport, []}]}]}]}]}]},
    ActionId = get_action_id(ProgressFilter),
    ct:pal("ActionId = ~p~n",[ActionId]),

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
		      [{brmBackupId, [], [BuId]},
		       {restore, []}]}]}]}]}]},
    ct:print("Executing action restore on backup ~p~n",[BuId]),
    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),
    {ok, ActionResponse} = netconf(action, [nc1, Action]),
    case extract_element(returnValue, ActionResponse) of
	{ok, {returnValue, _, ["0"]}} ->
	    ok;
	{ok, {returnValue, _, [ReturnValue]}} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)~n",[Error, ReturnValue]),
	    ct:fail(Error)
    end,

    ok = swmTestLib:check_action_capable_progress(
	   [{"WAIT", swmTestLib:map_current_action("restoreBackup")}]),
    case os:getenv("SIM_OR_TARGET") of
	"target" ->
	    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
	    net_kernel:disconnect(ErlNode),
	    ct:pal("Power cycle!~n",[]),
	    rct_power:cycle(node),
	    %% Wait for node to restart, to prevent error in ct-shell.
	    {ok, _} = ct_telnet:expect(console,
				       "Ericsson",
				       [{idle_timeout,180000},no_prompt_check]);
	_ ->
	    ct:pal("Force reboot!~n",[]),
	    call(init, reboot, []),
	    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
	    net_kernel:disconnect(ErlNode)
    end,

    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
	["FAILURE"] ->
	    ok = swmTestLib:wait_for_action_capable(),
	    ok;
	Result ->
	    ct:pal("restoreBackup: ~s~n",[Result]),
	    ct:fail(Result)
    end.




%%--------------------------------------------------------------------
%% @doc NodeUC443.N Delete backup
%% Using netconf to trig a backup to be removed
%% Initiate the action and wait for it to be deleted. Create a backup
%% if no one exists.
%% @end

%% delete_backup(Config) ->
%%     MeId = proplists:get_value(meId, Config),
%%     %% Backups = get_all_backups(MeId),
%%     Name =
%% 	case proplists:get_value(force_backup_name, Config, false) of
%% 	    true ->
%% 		proplists:get_value(backupName, Config);
%% 	    false ->
%% 		BName = "swm_backup_SUITE.delete_backup."++ftpes_test_lib:unique_name(),
%% 		NewConfig =
%% 		    lists:keyreplace(backupName, 1, Config,
%% 				     {backupName, BName}),
%% 		{ok, _} = create_backup(NewConfig),
%% 		BName
%% 	end,
%%     ct:pal("Selected backup ~p for deletion.~n",[Name]),

%%     ProgressFilter =
%% 	{'ManagedElement',
%% 	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
%% 	 [{managedElementId,[],[MeId]},
%% 	  {'SystemFunctions',
%% 	   [{systemFunctionsId,[],["1"]},
%% 	    {'BrM',
%% 	     [{brMId,[],["1"]},
%% 	      {'BrmBackupManager',
%% 	       [{brmBackupManagerId,[],["1"]},
%% 		{progressReport, []}]}]}]}]},
%%     ActionId = get_action_id(ProgressFilter),

%%     Action = {'ManagedElement',
%% 	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
%% 	      [{managedElementId,[],[MeId]},
%% 	       {'SystemFunctions',
%% 		[{systemFunctionsId,[],["1"]},
%% 		 {'BrM',
%% 		  [{brMId,[],["1"]},
%% 		   {'BrmBackupManager',
%% 		    [{brmBackupManagerId,[],["1"]},
%% 		     {'deleteBackup',[],
%% 		      [{name, [], [Name]}]}]}]}]}]},
%%     ct:pal("Executing action deleteBackup ~s~n",[Name]),
%%     ok = swmTestLib:wait_for_action_capable(60000*6),
%%     swmTestLib:flush_mnesia_event_messages(),
%%     case netconf(action, [nc1, Action]) of
%% 	{ok, ActionResponse} ->
%% 	    case extract_element(returnValue, ActionResponse) of
%% 		{ok, {returnValue, _, ["0"]}} ->
%% 		    ok;
%% 		{ok, {returnValue, _, [ReturnValue]}} ->
%% 		    Error = decode_return_value(ReturnValue),
%% 		    ct:pal("Return value is ~w (~s)~n",[Error, ReturnValue]),
%% 		    ct:fail(Error)
%% 	    end;
%% 	{error, ErrorResponse} ->
%% 	    {ok, {'error-message', _, [ErrorMessage]}} =
%% 		extract_element('error-message', ErrorResponse),
%% 	    Error = decode_error_message(ErrorMessage),
%% 	    ct:print("Action failure~n",[]),
%% 	    ct:log("~s~n",[ErrorMessage]),
%% 	    ct:fail(Error)
%%     end,
%%     case wait_for_progress(progressReport, ActionId, ProgressFilter) of
%% 	["SUCCESS"] ->
%% 	    ct:pal("Backup deleted successfully"),
%% 	    %% if called from clean backups, don't check
%% 	    case proplists:get_value(force_backup_name, Config, false) of
%% 		false ->
%% 		    ok = swmTestLib:check_action_capable_progress(
%% 			   [{"WAIT", swmTestLib:map_current_action("deleteBackup")},
%% 			    {"CAPABLE", ""}]),
%% 		    ok = swmTestLib:wait_for_action_capable();
%% 		_ ->
%% 		    ok
%% 	    end;
%% 	Result ->
%% 	    case proplists:get_value(forced, Config) of
%% 		true ->
%% 		    ct:pal("deleteBackup: ~s~n",[Result]);
%% 		false ->
%% 		    ct:pal("deleteBackup: ~s~n",[Result]),
%% 		    ct:fail(Result)
%% 	    end
%%     end.

delete_backup(Config) ->
    delete_backup_e(Config).

delete_backup_e(Config) ->
    MeId = proplists:get_value(meId, Config),

    Name =
	case proplists:get_value(force_backup_name, Config, false) of
	    true ->
		proplists:get_value(backupName, Config);
	    false ->
		BName = "swm_backup_SUITE.delete_backup."++
		    ftpes_test_lib:unique_name(),
		NewConfig =
		    lists:keyreplace(backupName, 1, Config,
				     {backupName, BName}),
		{ok, _} = create_backup_e(NewConfig),
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
    ok = swmTestLib:wait_for_action_capable_e(),
    swmTestLib:flush_mnesia_event_messages(),
%    flush_messages(),
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
	{ok, ["SUCCESS"], _} ->
	    ct:pal("Backup deleted successfully"),
	    %% if called from clean backups, don't check
	    case proplists:get_value(force_backup_name, Config, false) of
		false ->
		    ok = swmTestLib:check_action_capable_progress(
			   [{"WAIT", swmTestLib:map_current_action("deleteBackup")},
			    {"CAPABLE", ""}]),
		    ok = swmTestLib:wait_for_action_capable_e();
		_ ->
		    ok
	    end;
	{ok, Result, _} ->
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
    try delete_backup_e(NewConfig) of
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
	 delete_backup_e([{forced, true},
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

    %% Index =
    %%  case Backups of
    %%      [] ->
    %%      create_backup(Config),
    %%      [Element|_] = get_all_backups(MeId),
    %%      {'BrmBackup', _, BrmBackup} = Element,
    %%      {value, {brmBackupId, _, Id}} =
    %%          lists:keysearch(brmBackupId, 1, BrmBackup),
    %%      Id;
    %%      [Element|_] ->
    %%      {'BrmBackup', _, BrmBackup} = Element,
    %%      {value, {brmBackupId, _, Id}} =
    %%          lists:keysearch(brmBackupId, 1, BrmBackup),
    %%      Id
    %%  end,
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

    Uri = proplists:get_value(sftp_uri, Config),
    Password = proplists:get_value(sftp_pass, Config),
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
    RootDir = proplists:get_value(sftp_root, Config),
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
           backup_cancel(MeId),
           ct:fail(["FAILURE"]);
       true ->
           case wait_for_progress(progressReport, ActionId, ProgressFilter) of
            ["SUCCESS"] -> {ok, Files} = file:list_dir(RootDir),
                           ct:pal("RootDir = ~p ~nFiles = ~p~n",[RootDir, Files]),
                           ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("exportBackup")}, {"CAPABLE", ""}]),
                           ok = swmTestLib:wait_for_action_capable(),
                           ok;
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
    ct:pal("~p~n~p~n",[Name, Backups]),
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

    Uri = proplists:get_value(sftp_uri, Config),
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
	    ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("exportBackup")},
							   {"CAPABLE", ""}]),
	    ok = swmTestLib:wait_for_action_capable(),
	    ok;
	Result ->
	    ct:pal("exportBackup: ~s~n",[Result]),
	    ct:pal("URI: ~s~n",[Uri]),
	    cmd(["ls -la ",RootDir]),
	    ct:fail(Result)
    end.

export_failure(Config) ->
    export_failure_pass(Config).

%%--------------------------------------------------------------------
%% @doc NodeUC446.E1 Export failure
%% Using netconf to trig a backup to be exported
%% Initiate the action, and then check the progress report for completion
%% Access the backup on the export server, and verify the backup metadata
%% @end

export_failure_pass(Config) ->
    NewConfig = lists:keyreplace(sftp_pass, 1, Config, {sftp_pass, "wrongpassword"}),
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
    Uri = proplists:get_value(sftp_uri, Config),
    NewConfig = lists:keyreplace(sftp_uri, 1, Config, {sftp_uri, Uri ++ "/WrongComPath"}),
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

    Uri = proplists:get_value(sftp_uri, Config),
    Password = proplists:get_value(sftp_pass, Config),
    Cancel = proplists:get_value(cancel, Config, false),
    Action = {'ManagedElement',  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],[MeId]},
               {'SystemFunctions', 
		[{systemFunctionsId,[],["1"]},
		 {'BrM', 
		  [{brMId,[],["1"]},
		   {'BrmBackupManager', 
		    [{brmBackupManagerId,[],["1"]},
		     {autoExport, ["ENABLED"]},
		     {autoExportPassword, [], 
		      [{cleartext, [], []}, 
		       {password, [], [Password]}]},
		     {autoExportUri, [], [Uri]}]}]}]}]},
    RootDir = proplists:get_value(sftp_root, Config),
    %% check_server_conditions(Uri, Password),

    Ares = netconf(edit_config, [nc1, running, Action]),
    ok = swmTestLib:wait_for_action_capable_e(),
%    swmTestLib:flush_mnesia_event_messages(),
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
		{ok, ["SUCCESS"], _} -> 
		    ct:print("Unused messages ~p~n",
			     [process_info(self(), messages)]),
		    {ok, Files} = file:list_dir(RootDir),
		    ct:pal("RootDir = ~p ~nFiles = ~p~nName = ~p~n",
			   [RootDir, Files, Name]),
		    %% ok = swmTestLib:check_action_capable_progress(
		    %% 	   [{"WAIT", 
		    %% 	     swmTestLib:map_current_action(
		    %% 	       "manual_exportBackup")}, 
		    %% 	    {"CAPABLE", ""}]),
		    ok = swmTestLib:wait_for_action_capable_e(),
		    ok;
		{ok, Result, _} -> 
		    ct:pal("exportBackup: ~s~n",[Result]),
		    ct:pal("URI: ~s~n",[Uri]),
		    cmd(["ls -la ",RootDir]),
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
    NewConfig = lists:keyreplace(sftp_pass, 1, Config, {sftp_pass, "wrongpassword"}),
    try autoexport_backup(NewConfig) of
    ok ->
        ct:pal("The autoexport backup file transfer succeeded although it shouldn't"),
        ct:fail(export_failure_test_failed)
    catch exit:{test_case_failed, ["FAILURE"]} ->
        %%ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("exportBackup")},{"CAPABLE", ""}]),
        ok = swmTestLib:wait_for_action_capable_e(),
        ok
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC446.E1 AutoExport failure
%% Using netconf to trig a backup to be autoexported
%% Initiate the action, and then check the progress report for completion
%% Access the backup on the export server, and verify the backup metadata
%% @end

autoexport_failure_uri(Config) ->
    Uri = proplists:get_value(sftp_uri, Config),
    NewConfig = lists:keyreplace(sftp_uri, 1, Config, {sftp_uri, Uri ++ "/WrongComPath"}),
    try autoexport_backup(NewConfig) of
    ok ->
        ct:pal("The autoexport backup file transfer succeeded although it shouldn't"),
        ct:fail(export_failure_test_failed)
    catch exit:{test_case_failed, ["FAILURE"]} ->
        %%ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("exportBackup")},{"CAPABLE", ""}]),
        ok = swmTestLib:wait_for_action_capable_e(),
        ok
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC447.N Import Backup
%% Using netconf to cause the system to import a backup
%% Initiate the action and wait for it to be complete
%% Create and export a backup if none is available
%% @end

import_backup(Config) ->
    ImportName = find_import_file(Config),

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
    Uri = proplists:get_value(sftp_uri, Config) ++ "/" ++ ImportName,
    Password = proplists:get_value(sftp_pass, Config),
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
    ok = swmTestLib:wait_for_action_capable_e(),
    %% swmTestLib:flush_mnesia_event_messages(),
    {ok, _A} = netconf(action, [nc1, Action]),
    if Cancel ->
           backup_cancel(MeId),
           ct:fail(["FAILURE"]);
       true ->
           %% case wait_for_progress(progressReport, ActionId, ProgressFilter) of
	    case wait_for_progress_e(progressReport,ActionId,"BrmBackupManager") of

		{ok, ["SUCCESS"], _} ->
		    ct:pal("Backup succesfully imported."),
		    ok = swmTestLib:wait_for_action_capable_e();
		{ok, Result,_} -> 
		    ct:pal("importBackup: ~s~n",[Result]),
		    ct:fail(Result)
           end
    end.

find_import_file(Config) ->
    case existing_backups(Config) of
	[] ->
	    MeId = proplists:get_value(meId, Config),
	    Set =
		{'ManagedElement',
		 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		 [{managedElementId,[],[MeId]},
		  {'SystemFunctions',
		   [{systemFunctionsId,[],["1"]},
		    {'BrM',
		     [{brMId,[],["1"]},
		      {exportPackageLabelPrefix, ["RbsBackup"]}]}]}]},
	    ok = netconf(edit_config, [nc1, running, Set]),
	    export_backup(Config),
	    ImportFile = find_import_file(Config),
	    %% Make sure this backup isn't on the node
	    clean_backups(Config),
	    ImportFile;
	[ImportFile|_] ->
	    %% Make sure this backup isn't on the node
	    clean_backups(Config),
	    ImportFile
    end.

%%--------------------------------------------------------------------
existing_backups(Config) ->
    Dir = proplists:get_value(sftp_root, Config),
    case file:list_dir(Dir) of
	   {ok, Files} ->
           [File || File <- Files, lists:prefix("RbsBackup", File)];
	   {error, Reason} ->
           ct:pal("list_dir(~p) ~p~n", [Dir, file:format_error(Reason)]),
	       ct:fail(Reason)
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC447.E1 Import failure
%% Using netconf to trig an import of a backup but modify password to
%% make the import fail. Make sure the system reponds correctly
%% @end

import_failure_pass(Config) ->
    NewConfig = lists:keyreplace(sftp_pass, 1, Config, {sftp_pass, "wrongpassword"}),
    try import_backup(NewConfig) of
	ok ->
	    ct:pal("The backup file transfer succeeded although it shouldn't"),
	    ct:fail(import_failure_test_failed)
    catch
	exit : {test_case_failed, ["FAILURE"]} ->
	    timer:sleep(1000),
	    ok = swmTestLib:wait_for_action_capable_e()
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC447.E1 Import failure
%% Using netconf to trig an import of a backup but modify URI to
%% make the import fail. Make sure the system reponds correctly
%% @end

import_failure_uri(Config) ->
    Uri = proplists:get_value(sftp_uri, Config),
    NewConfig = lists:keyreplace(sftp_uri, 1, Config, {sftp_uri, Uri ++ "/WrongComPath"}),
    try import_backup(NewConfig) of
    ok ->
        ct:pal("The backup file transfer succeeded although it shouldn't"),
        ct:fail(import_failure_test_failed)
    catch
	exit : {test_case_failed, ["FAILURE"]} ->
	    timer:sleep(1000),
	    ok = swmTestLib:wait_for_action_capable_e(),
	    ok
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC447.E1 Import cancel
%% Using netconf to trig an import of a backup but modify parameters to
%% make the import fail. Make sure the system reponds correctly
%% @end

import_backup_cancel(Config) ->
    try import_backup([{cancel, true} | Config]) of
    ok ->
        ct:pal("The backup file transfer succeeded although it shouldn't"),
        ct:fail(import_failure_test_failed)
    catch
	exit : {test_case_failed, ["FAILURE"]} ->
	    timer:sleep(1000),
	    ok = swmTestLib:wait_for_action_capable_e()
    end.

%%--------------------------------------------------------------------
%% @doc Internal housekeeping
%% Make sure internal housekeeping removes system created backups
%% @end

internal_housekeeping(Config) ->
    MeId = proplists:get_value(meId, Config),

    %% Run housekeeping
    ct:pal("Executing internal housekeeping",[]),
    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),
    call(swmLib, set_variable, [swm_test_housekeeping_delay, 0]),
    call(swmBackup, internal_housekeeping, []),   
    ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("internal_housekeeping")},
                                                   {"CAPABLE", ""}]),

    OriginalBackups = get_all_backups(MeId),
    list_backups(OriginalBackups),
    Size = length(OriginalBackups),
    ct:pal("Original number of backups = ~p~n",[Size]),

    %% Force a system created backup
    ct:pal("Create an unused system created backup", []),
    make_system_backup(
      Config, "swm_backup_SUITE.internal_housekeeping."++ftpes_test_lib:unique_name()),

    %% There should now be one more backup
    check_number_of_backups(MeId, Size+1),

    %% Run housekeeping
    ct:pal("Executing internal housekeeping",[]),
    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),
    call(swmLib, set_variable, [swm_test_housekeeping_delay, 0]),
    call(swmBackup, internal_housekeeping, []),
    ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("internal_housekeeping")},
                                                   {"CAPABLE", ""}]),

    %% There should now be the original number of backups
    check_number_of_backups(MeId, Size),


    %% Activate the failsafe function
    ct:pal("Activating failsafe function to make yet another system "
	   "created backup",[]),
    activate_failsafe(Config),
    ct:pal("Failsafe activated",[]),

    %% Run housekeeping
    ct:pal("Executing internal housekeeping",[]),
    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),
    call(swmLib, set_variable, [swm_test_housekeeping_delay, 0]),
    call(swmBackup, internal_housekeeping, []),
    ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("internal_housekeeping")},
                                                   {"CAPABLE", ""}]),

    %% There should still be one more backup
    check_number_of_backups(MeId, Size+1),


    %% Deactivate failsafe
    ct:pal("Deactivating failsafe",[]),
    deactivate_failsafe(Config),
    ct:pal("Failsafe deactivation complete",[]),

    %% There should now be no additional backups
    check_number_of_backups(MeId, Size),
    call(swmLib, erase_variable, [swm_test_housekeeping_delay]),
    ok.


%%--------------------------------------------------------------------
%% @doc Restore backup prior to upgrade
%% Make sure that it is possibe to restore to the backup that was made prior to backup
%% @end
restore_backup_before_upgrade(_Config) ->
    ok.


%%%--------------------------------------------------------------------
%%% LIBRARY FUNCTIONS
%%%--------------------------------------------------------------------

list_backups(Backups) ->
    ct:pal(generate_backup_list(Backups),[]).

generate_backup_list(Backups) ->
    [begin
	 {ok, {_, _, [Name]}} = extract_element(backupName, [Backup]),
	 {ok, {_, _, [CreationType]}} = 
	     extract_element(creationType, [Backup]),
	 io_lib:format("~s ~s~n",[Name, CreationType])
     end||Backup<-Backups].

check_number_of_backups(MeId, Expected) ->
    BuList = get_all_backups(MeId),
    Current = length(BuList),
    ct:pal("Backup check:~n------------------------------~n"++
	       generate_backup_list(BuList)++
	       "~n------------------------------~n"
	   "Number of backups: ~p~nExpected: ~p~n",
	   [Current, Expected]),
    case Current of 
	Expected ->         
	    ok;
	_ -> 
	    ct:fail(housekeeping_no_of_backups_fail)
    end.


%%%--------------------------------------------------------------------
%%% Description: Create backups until there the given number of backups exist
%%%--------------------------------------------------------------------


create_backups(Config, Number) ->
    MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config),
    Backups = [X||X<-get_all_backups(MeId),
		  case extract_element(creationType, [X]) of
		      {ok, {creationType, _, ["MANUAL"]}} ->
			  true;
		      _ ->
			  false
		  end],

    Current = length(Backups),
    ct:pal("Current number of backups: ~w of ~w~n",[Current, Number]),
    case Current of
	Current when Current<Number ->
	    NewName = Name++"."++ftpes_test_lib:unique_name(),
	    NewConfig =
		lists:keyreplace(backupName, 1, Config, {backupName, NewName}),
	    create_backup(NewConfig),
	    create_backups(Config, Number);
	_ ->
	    ok
    end.

%%%--------------------------------------------------------------------
%%% Description: Make a fake system created backup
%%%--------------------------------------------------------------------

make_system_backup(_, BackupName) ->
    %% Use whitebox access to create an unused system backup
    Type = system,
    MoRef = call(swmBackup, test_create_backup, [Type, BackupName]),
    ct:pal("MoRef = ~p~n",[MoRef]),

    Index = lists:last(string:tokens(MoRef, "=")),
    call(swmLib, unlock_backup, [Index]),
    ok.

%%%--------------------------------------------------------------------
%%% Description: Activate failsafe
%%%--------------------------------------------------------------------

activate_failsafe(Config) ->
    MeId = proplists:get_value(meId, Config),
    Activate = {'ManagedElement',
		[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		[{managedElementId,[],[MeId]},
		 {'SystemFunctions',
		  [{systemFunctionsId,[],["1"]},
		   {'BrM',
		    [{brMId,[],["1"]},
		     {'BrmBackupManager',
		      [{brmBackupManagerId,[],["1"]},
		       {'BrmFailsafeBackup',
			[{brmFailsafeBackupId, [], ["1"]},
			 {'activate',[]}]}]}]}]}]},
    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),
    ct:pal("Executing action activate"),
    {ok, ActionResponse} = netconf(action, [nc1, Activate]),
    case extract_element(returnValue, ActionResponse) of
	{ok, {returnValue, _, ["0"]}} ->
	    ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("ACTIVATE")},
							   {"CAPABLE", ""},
							   {"WAIT", swmTestLib:map_current_action("internal_housekeeping")},
							   {"CAPABLE", ""}]),
	    ok = swmTestLib:wait_for_action_capable(),
	    ok;
	{ok, {returnValue, _, [ReturnValue]}} ->
	    ct:pal("Return value is ~s~n",[ReturnValue]),
	    ct:fail({failsafe, ReturnValue})
    end.

%%%--------------------------------------------------------------------
%%% Description: Deactivate failsafe
%%%--------------------------------------------------------------------


deactivate_failsafe(Config) ->
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
		{'BrmFailsafeBackup',
		 [{brmFailsafeBackupId, [], ["1"]},
		  {progress, []}]}]}]}]}]},

    Deactivate = {'ManagedElement',
		  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		  [{managedElementId,[],[MeId]},
		   {'SystemFunctions',
		    [{systemFunctionsId,[],["1"]},
		     {'BrM',
		      [{brMId,[],["1"]},
		       {'BrmBackupManager',
			[{brmBackupManagerId,[],["1"]},
			 {'BrmFailsafeBackup',
			  [{brmFailsafeBackupId, [], ["1"]},
			   {'deactivate',[]}]}]}]}]}]},
    ActionId = get_action_id(ProgressFilter),
    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),
    ct:pal("Executing action deactivate (OldActionId = ~p)",[ActionId]),
    {ok, _} = netconf(action, [nc1, Deactivate]),
    case wait_for_progress(progress, ActionId, ProgressFilter) of
	["SUCCESS"] ->
	    ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("DEACTIVATE")},
							   {"CAPABLE", ""}]),
	    ok = swmTestLib:wait_for_action_capable(),
	    ct:pal("Failsafe deactivation complete");
	Result ->
	    ct:pal("deactivate_failsafe: ~s~n",[Result]),
	    ct:fail(Result)
    end.





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


%%%--------------------------------------------------------------------
%%% Description: Netconf
%%%--------------------------------------------------------------------

netconf(F, A) ->
    %% ct:print("netconf ~w ~p~n",[F, A]),
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
%%% Description: Construct sftp
%%%--------------------------------------------------------------------

sftp_uri(Config) ->
    Host = proplists:get_value(sftp_host, Config),
    User = proplists:get_value(sftp_user, Config),
    Dir = proplists:get_value(sftp_root, Config),
    "sftp://"++User++"@"++Host++Dir.

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
			    ct:pal("Waiting for updated progress ~s~n~p~n",
				   [OldActionId, Report]),
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
	    

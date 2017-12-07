%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmt_integration_trusted_ai_backup_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R7A/R8A/1
%%%
%%% @doc == AI using backup.==
%%%
%%%
%%% @end

-module(lmt_integration_trusted_ai_backup_SUITE).
-author('etxmlar').
-vsn('/main/R7A/R8A/1').
-date('2017-01-24').

%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
%%% R7A/1     2016-11-02  etxmlar     Created
%%% R7A/2     2016-11-02  etxmlar     Updated and checked in
%%% R8A/1     2017-01-20  etxmlar     Added support for new boardtypes
%%% ----------------------------------------------------------
%%%

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 groups/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 lmt_integration_ai_load_backup_ipv4/1,
	 lmt_integration_ai_load_backup_ipv6/1
	]).


-define(NC, nc1).
-define(CLI, cli).
-define(BU_NAME, "test").
-define(TFTPBOOT_DIR, "/proj/rcs-tmp/tftpboot/").
-define(Protocol, "https").
-define(DefaultPort, "8080").
%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% @end
%%--------------------------------------------------------------------
suite() ->
    case nl_lib:check_if_vc_board() of
	"yes" -> 
	    [{timetrap,{minutes,60}},
	     {ct_hooks, [{rct_htmllink,[]},
			 {rct_power,power},
			 {rct_consserv,cs1},
			 {rct_netconf, {nc1, man_auth}},
			 {rct_cli, {cli, [{user, "SysAdminTest"}, {password, "SysAdminTest"},manual_connect]}},
			 {rct_ssh,{ssh,[manual_connect]}},
			 {rct_rs232,console},
			 {cth_conn_log,[]}]}];
	_Other ->
	    [{timetrap, {minutes, 60}}, % 1 hours
	     {ct_hooks, [{rct_rpc, rpc},
			 {rct_htmllink,[]},
			 {rct_consserv,cs1},
			 {rct_rs232,console},
			 {rct_power,node},
			 {rct_netconf, nc1},
			 {rct_cli, {cli, [manual_connect]}},
			 {cth_conn_log, []},
			 %% {rct_logging, {all, []}},
			 %% {rct_core,[]}
			 {rct_cli, {cli, [manual_connect]}}
			]}]
    end.


%% @hidden
init_per_suite(Config) ->

    [{_, NodeName}] = ct:get_config(test_nodes),
    Node_Name = atom_to_list(NodeName),
    ct:pal("NodeName: ~p",[Node_Name]),

    BoardType = get_boardtype(),

    [{nodename, Node_Name},
     {board_type, BoardType}| Config].
    %% Config.
%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.
%% @hidden
init_per_testcase(TestCase, Config)  
  when TestCase == lmt_integration_ai_load_backup_ipv4 ->

    %% Check what TN_port to use depending on boardtype
    BoardType = proplists:get_value(board_type, Config),
    TN_port = nl_lib:get_tn_port_capitel(BoardType),

    SiteInstallationFile = "SiteInstallationFileIpv4.xml",
    [{sif, SiteInstallationFile},
     {tn_port, TN_port}|Config];

init_per_testcase(TestCase, Config) 
  when TestCase == lmt_integration_ai_load_backup_ipv6 ->

    %% Check what TN_port to use depending on boardtype
    BoardType = proplists:get_value(board_type, Config),
    TN_port = nl_lib:get_tn_port_capitel(BoardType),

    SiteInstallationFile = "SiteInstallationFileIpv6.xml",
    [{sif, SiteInstallationFile},
     {tn_port, TN_port}| Config];

init_per_testcase(TestCase, Config) ->
    ct:pal("TestCase: ~p",[TestCase]),
    Config.



%% @hidden
end_per_testcase(_TestCase, Config) ->

    NodeName = proplists:get_value(nodename, Config),
    os:cmd("cp "++?TFTPBOOT_DIR++NodeName++"/org_RbsSummaryFile.xml " ++ 
    	       ?TFTPBOOT_DIR++NodeName++"/RbsSummaryFile.xml"),

    os:cmd("rm "++?TFTPBOOT_DIR++NodeName++"/org_RbsSummaryFile.xml "),
    os:cmd("rm "++?TFTPBOOT_DIR++NodeName++"/mod_RbsSummaryFile.xml "),


    %%Remove SIF in priv dir
    SiteInstallationFile = proplists:get_value(sif, Config),
    remove_sif_in_priv_dir(Config, SiteInstallationFile),
   
    %% Install orginal.
    ct:pal("Install original.", []),
    nl_lib:board_restore(Config, console, ?NC, ?Protocol),  
    nl_lib:download_files(Config, console, ?Protocol),  
    nl_lib:integrate(Config, console, ?NC, ?Protocol), 
    case proplists:get_value(tc_status, Config) of
	ok ->
    	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p. Export logs. \n", [Reason]),
	    nl_lib:export_ai_log(Config, ?Protocol),
	    nl_lib:export_esi(Config, ?Protocol)
    end,
    ok.
%%--------------------------------------------------------------------
%% @doc
%% groups.
%% @end
%%--------------------------------------------------------------------
groups() ->
    [
    ].

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [lmt_integration_ai_load_backup_ipv4,
     lmt_integration_ai_load_backup_ipv6
    ].

%%--------------------------------------------------------------------
%% @doc 
%% AI load backup using lmt integration ipv4<br/><br/>
%% @end
%%--------------------------------------------------------------------
lmt_integration_ai_load_backup_ipv4(Config) ->
    ct:log("TC: xx, lmt_integration trusted ipv4 usisng backup"),
    NodeName = proplists:get_value(nodename, Config),

    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    ct:pal("LogPath: ~p", [LogPath] ),

    %%Create backup
    ok = create_backup(),
    timer:sleep(30000), %% Make sure backup is created.

    %%Export backup
    ok = export_backup(LogPath),

    BuName = get_exported_bu(LogPath),
    true = modify_rbs_summary_file(NodeName, BuName, LogPath),

    %%Create SIF
    SiteInstallationFile =  proplists:get_value(sif, Config),
    TN_port_capitel = proplists:get_value(tn_port, Config),
    generate_sif_ipv4_trusted(Config, SiteInstallationFile, TN_port_capitel),
    
    %% Board restore
    aic_httpc:board_restore(Config, console),
    ct:log("Board is restored. Now test can start."),
    timer:sleep(5000),

    %% Board Download
    nl_lib:httpc_request_lmt_integration(Config, post, "Download", ?Protocol, 
					 ?DefaultPort, SiteInstallationFile),

    %%Get the boards TN port
    BoardType = proplists:get_value(board_type, Config),
    ct:log("BoardType: ~p", [BoardType]),

    TN_port =  nl_lib:get_tn_port_lower_case(BoardType),

    case  ct_telnet:expect(console,
			   "Enabling Northbound interface: "++TN_port, 
    			   [{timeout,120000},no_prompt_check]) of	    
    	{ok, _} ->
    	    {ok, _} = ct_telnet:expect(console,
    				       TN_port++" enabled", 
    				       [{timeout,60000},no_prompt_check]),

	    {ok, _} = ct_telnet:expect(console,
				       "Vlan Interface Added", 
				       [{timeout,60000},no_prompt_check]),

	    {ok, _} = ct_telnet:expect(console,
				       "Default router added", 
				       [{timeout,60000},no_prompt_check]),

	    {ok, _} = ct_telnet:expect(console,
				       TN_port++" configured", 
				       [{timeout,60000},no_prompt_check]);
    	_Else ->
	    ct:fail(TN_port++" not enabled when Download files")
    end,
    
    nl_lib:wait_for_download_completed(Config, ?Protocol),
    ct:log("# Download done: LMT Integration."),
    timer:sleep(5000),

    %% Integrate
    aic_httpc:integrate_lmt_integration(Config, console, nc1),

    true = check_backup_is_used(),

    ok.
%%--------------------------------------------------------------------
%% @doc 
%% AI load backup using lmt integration ipv6<br/><br/>
%% @end
%%--------------------------------------------------------------------
lmt_integration_ai_load_backup_ipv6(Config) ->
    ct:log("TC: xx, lmt_integration trusted ipv6 usisng backup"),
    NodeName = proplists:get_value(nodename, Config),

    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    ct:pal("LogPath: ~p", [LogPath] ),

    ok = create_backup(),
    timer:sleep(30000), %% Make sure backup is created.

    ok = export_backup(LogPath),

    BuName = get_exported_bu(LogPath),
    true = modify_rbs_summary_file(NodeName, BuName, LogPath),

    %%Create SIF
    SiteInstallationFile =  proplists:get_value(sif, Config),
    TN_port_capitel = proplists:get_value(tn_port, Config),
    generate_sif_ipv6_trusted(Config, SiteInstallationFile, TN_port_capitel),

    %% Board restore
    aic_httpc:board_restore(Config, console),
    ct:log("Board is restored. Now test can start."),
    timer:sleep(5000),

    %% Board Download
    nl_lib:httpc_request_lmt_integration(Config, post, "Download", ?Protocol, 
					 ?DefaultPort, SiteInstallationFile),

    %%Get the boards TN port
    BoardType = proplists:get_value(board_type, Config),
    ct:log("BoardType: ~p", [BoardType]),

    TN_port =  nl_lib:get_tn_port_lower_case(BoardType),

    case  ct_telnet:expect(console,
			   "Enabling Northbound interface: "++TN_port, 
    			   [{timeout,120000},no_prompt_check]) of	    
    	{ok, _} ->
    	    {ok, _} = ct_telnet:expect(console,
				       TN_port++" enabled", 
    				       [{timeout,60000},no_prompt_check]),

	    {ok, _} = ct_telnet:expect(console,
				       "Vlan Interface Added", 
				       [{timeout,60000},no_prompt_check]),

	    {ok, _} = ct_telnet:expect(console,
				       "Default router added", 
				       [{timeout,60000},no_prompt_check]),

	    {ok, _} = ct_telnet:expect(console,
				       TN_port++" configured", 
				       [{timeout,60000},no_prompt_check]);
    	_Else ->
	    ct:fail(TN_port++" not enabled when Download files")
    end,
    
    nl_lib:wait_for_download_completed(Config, ?Protocol),
    ct:log("# Download done: LMT Integration."),
    timer:sleep(5000),

    %% Integrate
    aic_httpc:integrate_lmt_integration(Config, console, nc1),

    true = check_backup_is_used(),

    ok.



%% ------------------------
%% Internal
%% ------------------------
create_backup() ->
    wait_for_cli_is_up(),
    ok = rct_cli:connect(cli),
    {ok,_} = rct_cli:send(cli,"ManagedElement=1,SystemFunctions=1,BrM=1,BrmBackupManager=1"),
    {ok,_} = rct_cli:send(cli,"createBackup "++?BU_NAME),
    ok = rct_cli:disconnect(cli).

wait_for_cli_is_up() ->
    wait_for_cli_is_up(60000).
wait_for_cli_is_up(Timeout) when Timeout < 0 ->

    ct:fail("Could not open cli session within max timeout!");
wait_for_cli_is_up(Timeout) ->
    case rct_cli:connect(cli) of
	ok ->
	    rct_cli:disconnect(cli);
	_Else ->
	    rct_cli:disconnect(cli),
	    ct:pal("Cli not up, sleep and try again.",[]),
	    timer:sleep(10000),
	    wait_for_cli_is_up(Timeout-10000)
    end.

export_backup(LogPath) ->
    ok = rct_cli:connect(cli),
    {ok, _} = rct_cli:send(cli,"ManagedElement=1,SystemFunctions=1,BrM=1,BrmBackupManager=1"),
    {ok, A} = rct_cli:send(cli,"show"),
    ct:log("A: ~p",[A]),
    %% cleanup str.
    B = string:tokens(A," \r\n[\"]"),
    ct:log("B: ~p",[B]),

    BackupList = [S || "BrmBackup=" ++ _ = S <- B],
    ct:log("BackupList: ~p",[BackupList]),

    LatestBu = lists:last(BackupList),
    ct:pal("LatestBu: ~p",[LatestBu]),

    {ok, D} = rct_cli:send(cli,"show "++LatestBu),
    ct:log("D: ~p",[D]),

    [{host, SftpHost},
     {username, Username},
     {password, Password}] = ct:get_config(sftp_server),

    URI = "sftp://"++Username++"@"++SftpHost,

    {ok, _} = rct_cli:send(cli, LatestBu),
    {ok, E} = rct_cli:send(cli, "export "++URI++LogPath++" "++Password),
    ct:log("E: ~p",[E]),

    timer:sleep(10000),
    check_export_result(),

    ok = rct_cli:disconnect(cli).    

modify_rbs_summary_file(NodeName, BuName, LogPath) ->
    Dir = ?TFTPBOOT_DIR ++ NodeName++"/",
    os:cmd("cp "++Dir++"RbsSummaryFile.xml " ++ Dir++"org_RbsSummaryFile.xml"),

    BackupFilePath = LogPath++BuName,
    ct:log("BackupFilePath: ~p",[BackupFilePath]),

    %% Modify RbsSummaryFile.xml to use backup.
    %% Add \ before " and \ in cmd to make it an valid erlan str.
    Sed_CMD = "sed -e \"s|.*siteBasicFilePath=.*|backupFilePath='"++BackupFilePath++"'|\" "
	"-e '/siteEquipmentFilePath=/d' "
	"-e '/licensingKeyFilePath=/d' "
	"-e '/labConfigFilePath=/d' "
	"-e '/initialSecurityConfigurationFilePath=/d' "
	"-e 's|upgradePackageFilePath=\\(.*\\)|upgradePackageFilePath=\\1/>|' "
	++Dir++"RbsSummaryFile.xml > "++Dir++"mod_RbsSummaryFile.xml",

    ct:log("Sed_CMD: ~s",[Sed_CMD]),
    os:cmd(Sed_CMD),

    os:cmd("cp "++Dir++"mod_RbsSummaryFile.xml "++Dir++"RbsSummaryFile.xml"),

    A = string:tokens(os:cmd("cat "++Dir++"RbsSummaryFile.xml"), " \r[\"]"),
    ct:pal("Cat: ~s",[A]),

	%% check that RbsSummaryFile contains something
	os:cmd("stat --printf=\"%s\" RbsSummaryFile.xml")>0.

get_exported_bu(LogPath) ->
    Ls = os:cmd("ls "++LogPath),
    ct:log("Ls: ~p",[Ls]),
    %%{match, _} = re:run(Ls,"_"++?BU_NAME++"_"),
    {match, _} = re:run(Ls,?BU_NAME++"_"), 
    %% cleanup str.
    LsList = string:tokens(Ls,"\n"),
    ct:log("LsList: ~p",[LsList]),

    %%BackupList = [S || "_test_" ++ _ = S <- LsList],
    BackupList = [S || "test_" ++ _ = S <- LsList],
    ct:log("BackupList: ~p",[BackupList]),

    %% Simple check that only one backup exist.
    1 = length(BackupList),

    [BackupStr] = BackupList,
    ct:pal("Backup: ~p",[BackupStr]),
    BackupStr.

check_export_result() ->
    check_export_result(120000).
check_export_result(Timeout) when Timeout < 0 ->
    ct:fail("TC fail due to no backup exported within time. ");
check_export_result(Timeout) ->
    {ok, AA} = rct_cli:send(cli,"show"),
    ct:log("AA: ~p",[AA]),
    %% cleanup str.
    BB = string:tokens(AA," \r\n[\"]"),
    ct:log("BB: ~p",[BB]),
    
    CC = lists:dropwhile(fun(X) ->
				 X =/= "result=SUCCESS"
			 end, BB),
    ct:log("CC: ~p",[CC]),
    
    case CC of
	[] ->
	    %% ct:fail("TC will fail due to no success.");
	    timer:sleep(10000),
	    check_export_result(Timeout-10000);
	_Other ->
	    ok
    end.
	    

check_backup_is_used() ->

    ct:log("Check Backup is used"),

    ok = rct_cli:connect(cli),
    {ok, AA} = rct_cli:send(cli,"show ManagedElement=1,SystemFunctions=1,BrM=1,"
			    "BrmBackupManager=1,BrmBackupLabelStore=1,lastRestoredBackup"),
    rct_cli:disconnect(cli),
    ct:log("AA: ~p",[AA]),
    %% cleanup str.
    LastRestoredBackup = string:tokens(AA," \r\n[\"]>"),
    ct:log("RestoredBackup: ~p",[LastRestoredBackup]),
    
    CCC = lists:dropwhile(fun(X) ->
				 X =/= "lastRestoredBackup="
			 end, LastRestoredBackup),
    ct:pal("CCC: ~p",[CCC]),
    ["lastRestoredBackup=",?BU_NAME] == CCC.
    

generate_sif_ipv4_trusted(Config, SiteInstallationFile, TN_port)->

    ct:log("Generate SIF ipv4 and write to priv dir"),  
    HwId = list_to_atom(proplists:get_value(nodename, Config)),
    ct:pal("Hwid: ~p",[HwId]),

    ok = nl_lib:gen_and_write_sif_ipv4_trusted(Config,
					       SiteInstallationFile, 
					       "RbsSummaryFile.xml", 
					       HwId, TN_port),  

    %% Check SIF exist in priv dir
    Priv_dir = ?config(priv_dir, Config),
    Ls = string_token(os:cmd("ls "++Priv_dir)),

    ct:pal("ls : ~p ", [Ls]),
    true = lists:member(SiteInstallationFile, Ls).


generate_sif_ipv6_trusted(Config, SiteInstallationFile, TN_port)->

    ct:log("Generate SIF ipv6 and write to priv dir"), 
    HwId = list_to_atom(proplists:get_value(nodename, Config)),
    ct:pal("Hwid: ~p",[HwId]),

    ok = nl_lib:gen_and_write_sif_ipv6_trusted(Config,
					       SiteInstallationFile, 
					       "RbsSummaryFile.xml", 
					       HwId, TN_port),  

    %% Check SIF exist in priv dir
    Priv_dir = ?config(priv_dir, Config),
    Ls = string_token(os:cmd("ls "++Priv_dir)),

    ct:pal("ls : ~p ", [Ls]),
    true = lists:member(SiteInstallationFile, Ls).

remove_sif_in_priv_dir(Config, SiteInstallationFile)->

    Priv_dir = ?config(priv_dir, Config),
    RmCmd = "rm "++Priv_dir++SiteInstallationFile,
    Rm = os:cmd(RmCmd),
    ct:log("Rm: ~p ", [Rm]),

    Ls = string_token(os:cmd("ls "++Priv_dir)),
    ct:log("ls : ~p ", [Ls]),							
    ok.

string_token(Str) ->
    ListOfStr = string:tokens(Str, "\n"),
    ListOfStr.

get_boardtype() ->
    BoardType = proplists:get_value(board_type,
				    ct:get_config(
				      ct:get_config({test_nodes,1}))),
    ct:log("BoardType: ~p", [BoardType]),
    BoardType.

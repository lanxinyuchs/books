%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_br_2_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R4A/R5A/R7A/R9A/1
%%% 
%%% @doc == TestSuite for testing backup restore with HW swap.==
%%% <br/><br/>
%%% rct_cli is used in ct_hooks:
%%% see <a href="file:///vobs/rcs/test/RCT_CRX901275/test/lib/supp/esrc/rct_cli.erl">rct_cli.erl</a><br/>
%%% @end

-module(swm_br_2_SUITE).
-vsn('/main/R4A/R5A/R7A/R9A/1').
-author('etxivri').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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
%%% R4A/1      2015-10-16  etxmlar     Created
%%% R4A/2      2015-11-27  etxmlar     Updated
%%% R5A/1      2015-11-27  etxmlar     Updated wait_time
%%% R5A/2      2016-01-08  etxmlar     Incresed timetrap
%%% R7A/2      2016-09-20  etxivri     A try to make it more robust in R7.
%%% R9A/1      2017-03-24  etxivri     A try to make import backup more robust.
%%% ----------------------------------------------------------
%%% 

%compile([export_all]).
-include_lib("common_test/include/ct.hrl").
-include_lib("xmerl/include/xmerl.hrl").



-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 groups/0
	]).

-export([configure_tls/1,
	 connect_cli/1,
	 connect_netconf/1,
	 connect_rcscoli/1,
	 create_backup/1,
	 export_backup/1, 
	 import_backup/1, 
	 restore_backup/1
	]).

-define(CLI_Session, cli1).
-define(NC_sess, nc1).
-define(Data_Dir, "/vobs/rcs/test/RCT_CRX901275/test/suites/SWM/swm_br_2_SUITE_data/").
-define(BackUpName, "bu_swap_hw").
-define(EXP_IMP_DIR, "/proj/rcs-tmp/test_swm_brm/").
-define(SuiteName, "swm_br_2_SUITE").

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).

%% BRM defines
-define(BRM_LDN, "ManagedElement=1,SystemFunctions=1,BrM=1").
-define(SYS_LDN, "ManagedElement=1,SystemFunctions=1,").

%% TLS data
-define(TLS_DATA_DIR, "/vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/OMC/OMC_CNX9013315/test/suites/omc_tls_login_SUITE_data/").
%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [configure_tls,
     connect_cli, 
     connect_netconf, 
     connect_rcscoli,
     create_backup,
     export_backup,
     import_backup,
     restore_backup,
     delete_backup
    ].

groups() ->
    [{group_1,[],[configure_tls,
		  connect_cli, 
		  connect_netconf, 
		  connect_rcscoli,
		  create_backup,
		  export_backup
		 ]},
     {group_2,[],[import_backup,
		  restore_backup,
		  connect_cli, 
		  connect_netconf, 
		  connect_rcscoli
		 ]}
    ].

%%% ===========================================================================
%% group_1 scenario.
%% Install UP node A - done by jenkins. 
%%   1. Configure_tls,
%%   2. Connect_cli, 
%%   3. Connect_netconf, 
%%   4. Connect_rcscoli,
%%   5. Create backup.
%%   6. Export backup.
%%% ===========================================================================
%%% ===========================================================================
%%  group_2 scenario. 
%%  Install UP node B - done by jenkins.
%%   7. Import backup. 
%%   8. Restore backup.
%%   9. Connect_cli, 
%%  10. Connect_netconf, 
%%  11. Connect_rcscoli,
%%  Test Finished
%%% ===========================================================================

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    case check_if_vc_board() of
	"yes" -> [{timetrap, {hours, 3}},
		  {ct_hooks, [{rct_htmllink,[]},
			      {rct_rpc, rpc_1},
			      {rct_upgrade,ug1},
			      {rct_cli, {cli1,
			      		 [{user, "SysAdminTest"}, 
			      		  {password, "SysAdminTest"},
			      		  manual_connect]}},			    
			      {rct_netconf, {nc1, expert_auth}},
			      {cth_conn_log,[]},
			      {rct_logging, {all,
					     [{erlang,{["ERROR REPORT","CRASH REPORT"],
						       ["SSL: certify: ssl_handshake.erl:"]}}]}},
			      {rct_rs232,console}
			     ]}];
	_  ->
	    [{timetrap, {hours, 2}},
	     {ct_hooks, [{rct_htmllink,[]},
			 {rct_rpc, rpc_1},
			 {rct_rs232,console},
			 {rct_upgrade,ug1},
			 {rct_core,[]},
			 {rct_logging, 
			  {all,[{erlang,{["ERROR REPORT","CRASH REPORT"],
					 ["SSL: certify: ssl_handshake.erl:"]}}]}},
			 {rct_cli, {cli1, [manual_connect]}},
			 {rct_netconf, nc1, expert_auth}]}]
    end.


%% @hidden
init_per_suite(Config) ->

    %% For tls config and login (connect)
    crypto:start(),
    ssh:start(),
    ssl:start(),

    ct:pal("Preparing ct-loopdata for tls"),

    %%cp omc_tls_login_SUITE_data to swm_br_2_SUITE_data
    DataDir =  ?config(data_dir, Config),
    ct:log("DataDir: ~p", [DataDir]),

    A = os:cmd("cp "++?TLS_DATA_DIR++"* "++DataDir),
    ct:log("A: ~p", [A]),
    LS_Data_Dir = string_tokens(os:cmd("ls "++DataDir)),
    ct:log("Ls Data_Dir: ~p", [LS_Data_Dir]),

    %%Preparing config for tls
    NewConfig = prepare_config(Config),   
    ct:pal("Checking/updating cert files in my_priv_dir: ~p",
	   [?config(my_priv_dir, NewConfig)]),
    prepare_files(NewConfig), %copy needed cert-mtrl data_dir -> my_priv_dir

    %% Get activity_number
    TestActNr = ct:get_config({jenkins_config, test_activity_number}),
    %% Get Branch
    Branch = ct:get_config({jenkins_config, branch}),
    %% Get Type
    InstalledType = ct:get_config({jenkins_config, installed_type}),

    [{inst_type, InstalledType},
     {branch, Branch},
     {test_act_nr, TestActNr} 
     | NewConfig].

%% Config.
%% @hidden
end_per_suite(Config) ->
  
    DataDir =  ?config(data_dir, Config),
    ct:log("DataDir: ~p",[DataDir]),

    B = os:cmd("rm -rf "++DataDir++"*"),
    ct:log("end_per_suite B: ~p",[B]),
    ok.

%% @hidden
init_per_group(GroupName, Config) when GroupName == group_1 ->

    %%Get node name
    [{_, Node_Name}] = ct:get_config(test_nodes),
    NodeName =  atom_to_list(Node_Name),
    ct:pal("Node : ~p",[NodeName]),

    %%Make export dir
    A = os:cmd("mkdir "++?EXP_IMP_DIR++NodeName),
    ct:pal("A : ~p",[A]),

    %% Could exist other files
    B = os:cmd("rm "++?EXP_IMP_DIR++NodeName++"/*.zip"), 
    ct:log("B : ~p",[B]),

    [{nodeName, NodeName},
     {backupName, ?BackUpName},
     {sftp_host, ?SftpHost},
     {sftp_user, ?SftpUser},
     {sftp_pass, ?SftpPassword} | Config];

init_per_group(GroupName, Config) when GroupName == group_2 ->

    %%Get node name
    [{_, Node_Name}] = ct:get_config(test_nodes),
    NodeName =  atom_to_list(Node_Name),
    ct:pal("Node : ~p",[NodeName]),

    %%Make export dir
    A = os:cmd("mkdir "++?EXP_IMP_DIR++NodeName),
    ct:pal("A : ~p",[A]),

    %% Could exist other files
    B = os:cmd("rm "++?EXP_IMP_DIR++NodeName++"/*.zip"), 
    ct:log("B : ~p",[B]),

    [{nodeName, NodeName},
     {backupName, ?BackUpName},
     {sftp_host, ?SftpHost},
     {sftp_user, ?SftpUser},
     {sftp_pass, ?SftpPassword} | Config].

%% @hidden
end_per_group(GroupName, Config) when GroupName == group_1 ->

    %% Cleanup exported backup dir.
    NodeName = proplists:get_value(nodeName, Config),

    A = os:cmd("cd "++?EXP_IMP_DIR++NodeName),
    ct:log("A: ~p",[A]),
    os:cmd(["chmod a+rwx ", ?EXP_IMP_DIR++NodeName]),
    B = os:cmd("rm "++?EXP_IMP_DIR++NodeName++"/"++?SuiteName++"_*"),
    ct:log("B: ~p",[B]),
    ok;

%% @hidden
end_per_group(GroupName, Config) when GroupName == group_2 ->

    %% Cleanup exported backup dir.
    NodeName = proplists:get_value(nodeName, Config),

    A = os:cmd("cd "++?EXP_IMP_DIR++NodeName),
    ct:log("A: ~p",[A]),
    os:cmd(["chmod a+rwx ", ?EXP_IMP_DIR++NodeName]),
    B = os:cmd("rm "++?EXP_IMP_DIR++NodeName++"/"++?SuiteName++"_*"),
    ct:log("B: ~p",[B]),

    %% Remove backup from "jenkins"
    Branch = proplists:get_value(branch, Config),
    Activity_number = proplists:get_value(test_act_nr, Config),
    Type = atom_to_list(proplists:get_value(inst_type, Config)),
    
    Remove_CMD = 
	"/vobs/rcs/tools/RDE_LXA119945/tools/jenkins/rcs_backup.sh -B "++Branch++" -A "++Activity_number++" -T "++Type++" -r",

    ct:log("Remove_CMD:~n~p~n",[Remove_CMD]),
    C = os:cmd(Remove_CMD),
    ct:log("C : ~p",[C]),

    ok. 


%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(_TestCase, Config) ->
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p.  \n", [Reason])
    end,
    ok.

string_tokens(Str) ->
    string:tokens(Str, " \n").

%%% ===========================================================================
%%% @doc
%%% Create backup <br/>
%%% Using CLI to trig a backup to be created. <br/>
%%% @spec create_backup(Config) -> ok
%%% @end
%%% ===========================================================================
create_backup(Config) ->  

    ok = rct_cli:connect(?CLI_Session),

    %% Set exportPackageLabelPrefix
    {ok, _}  = set_attribute("exportPackageLabelPrefix ="++ ?SuiteName, ?BRM_LDN, check),

    %% Create backup
    BackUpName = proplists:get_value(backupName, Config),
    LdnString = ?BRM_LDN++",BrmBackupManager=1",
    ct:log("BackUpName: ~p~n",[BackUpName]),

    rct_cli:send(?CLI_Session, LdnString),

    {ok, RecievedData} = rct_cli:send(?CLI_Session, "createBackup --name "++BackUpName),

    Data = string:tokens(lists:flatten(RecievedData), "\r\n "),
    %%ct:log("create_backup Data = ~p~n",[Data]),
    ReturnValue = lists:nth(4, Data),

    case decode_return_value(ReturnValue) of
	actionStarted ->
	    ok;
	ErrorValue ->
	    ct:log("Create Backup error. Error: ~p~n",[ErrorValue]),
	    ct:fail(ErrorValue)
    end,  

    ok = wait_for_exp_progress_state(["actionName=\"CREATE\"","result=SUCCESS", "state=FINISHED"], 
				     ?BRM_LDN++",BrmBackupManager=1", 40), 

    ResultInfo = get_result_info(?BRM_LDN++" ,BrmBackupManager=1"),
    
    %% Ex. ResultInfo = ["resultInfo=BrM=1,BrmBackupManager=1,BrmBackup=1"]
    ct:log("Create backup, ResultInfo from progressreport: ~p~n",[ResultInfo]),
   	
    ok = rct_cli:disconnect(?CLI_Session),
    ok.


%%% ===========================================================================
%%% @doc
%%% Export backup <br/>
%%% Using CLI to trig a backup to be exported. <br/>
%%% @spec export_backup(Config) -> ok
%%% @end
%%% ===========================================================================
export_backup(Config) ->
    
    ok = rct_cli:connect(?CLI_Session),

    %% ResultInfo = BrmBackup MO to export (string) from progressreport
    ResultInfo = lists:flatten(get_result_info(?BRM_LDN++" ,BrmBackupManager=1")),
    BrmBackupMo = lists:subtract(ResultInfo, "resultInfo="),
    ct:log("Export backup, BackUpMO: ~p~n",[BrmBackupMo]),

    rct_cli:send(?CLI_Session, ?SYS_LDN++BrmBackupMo),

    Uri = sftp_uri(Config),
    Password = proplists:get_value(sftp_pass, Config),

    {ok, RecievedData} = rct_cli:send(?CLI_Session, "export --uri "++Uri++" --password "++Password),

    Data = string:tokens(lists:flatten(RecievedData), "\r\n "),
    %%ct:log("export_backup Data = ~p~n",[Data]),
    ReturnValue = lists:nth(7, Data),

    case decode_return_value(ReturnValue) of
	actionStarted ->
	    ok;
	ErrorValue ->
	    ct:log("Export Backup error. Error: ~p~n",[ErrorValue]),
	    ct:fail(ErrorValue)
    end,   

    ok = wait_for_exp_progress_state(["actionName=\"EXPORT\"", "result=SUCCESS", "state=FINISHED"], 
				     ?SYS_LDN++BrmBackupMo, 40), 


    NewResultInfo = lists:flatten(get_result_info(?SYS_LDN++BrmBackupMo)), 
    BackupFile = lists:subtract(NewResultInfo, "resultInfo="),
    %% resultInfo="/proj/rcs-tmp/test_swm_brm/dus5075/_bu_swap_hw_1_RCP-DUS2_20151021T145653+0000.zip" 
    ct:log("Export backup, BackupFile: ~p~n",[BackupFile]),

    %% Save backup to "Jenkins" 
    Branch = proplists:get_value(branch, Config),
    Activity_number = proplists:get_value(test_act_nr, Config),
    Node =  proplists:get_value(nodeName, Config),
    Type = atom_to_list(proplists:get_value(inst_type, Config)),
    
    Save_CMD = 
    	"/vobs/rcs/tools/RDE_LXA119945/tools/jenkins/rcs_backup.sh -B "++Branch++" -A "++Activity_number++" -N "++Node++" -T "++Type++" -b "++BackupFile,
    
    ct:log("Save_CMD:~n~p~n",[Save_CMD]),

    A = os:cmd(Save_CMD),
    ct:log("A:~n~p~n",[A]),
  
    %% Clean UP. Delete backup ++ remove exportPackageLabelPrefix
    BackUpName = proplists:get_value(backupName, Config),
    rct_cli:send(?CLI_Session, ?BRM_LDN++" ,BrmBackupManager=1"),
    {ok, _RecievedData} = rct_cli:send(?CLI_Session, "deleteBackup --name "++BackUpName),

    {ok, _} = set_attribute("no exportPackageLabelPrefix", ?BRM_LDN, no_check),

    ok = rct_cli:disconnect(?CLI_Session),
    ok.

%%% ===========================================================================
%%% @doc
%%% Import backup <br/>
%%% Using CLI to trig a backup to be imported. <br/>
%%% @spec import_backup(Config) -> ok
%%% @end
%%% ===========================================================================
import_backup(Config) ->

    ok = rct_cli:connect(?CLI_Session),

    %%Get backup from "Jenkins"
    Branch = proplists:get_value(branch, Config),
    Activity_number = proplists:get_value(test_act_nr, Config),
    NodeName =  proplists:get_value(nodeName, Config),
    Type = atom_to_list(proplists:get_value(inst_type, Config)),

    Get_CMD = 
    "/vobs/rcs/tools/RDE_LXA119945/tools/jenkins/rcs_backup.sh -B "++Branch++" -A "++Activity_number++" -N "++NodeName++" -T "++Type,
    ct:log("Get_CMD:~n~p~n",[Get_CMD]),

    BackUpFile = wait_for_backup(Get_CMD),
    ct:log("BackUpFile:~n~p~n",[BackUpFile]),

    %%Copy BackUpFile to SFTP server
    A = os:cmd("cp "++BackUpFile++" "++?EXP_IMP_DIR++NodeName),
    ct:log("A:~n~p~n",[A]),
 
    {ok, FileName} = file:list_dir(?EXP_IMP_DIR++NodeName),
    Uri = sftp_uri(Config)++"/"++FileName,
    Password = proplists:get_value(sftp_pass, Config),

    rct_cli:connect(?CLI_Session),
    rct_cli:send(?CLI_Session, ?BRM_LDN++" ,BrmBackupManager=1"),
    {ok, RecievedData} = rct_cli:send(?CLI_Session, "importBackup --uri "++Uri++" --password "++Password),

    Data = string:tokens(lists:flatten(RecievedData), "\r\n "),
    %%ct:log("import_backup Data = ~p~n",[Data]),
    ReturnValue = lists:nth(8, Data),

    case decode_return_value(ReturnValue) of
	actionStarted ->
	    ok;
	ErrorValue ->
	    ct:log("Import Backup error. Error: ~p~n",[ErrorValue]),
	    ct:fail(ErrorValue)
    end,
     
    ok = wait_for_exp_progress_state(["actionName=\"IMPORT\"", "result=SUCCESS", "state=FINISHED"], 
				     ?BRM_LDN++" ,BrmBackupManager=1", 40), 


    ResultInfo = lists:flatten(get_result_info(?BRM_LDN++" ,BrmBackupManager=1")),
    BackupName = lists:subtract(ResultInfo, "resultInfo="),
    %% Ex. resultInfo="BrM=1,BrmBackupManager=1,BrmBackup=2" 
    ct:log("Import backup, BackupName: ~p~n",[BackupName]),

    ok = check_data_from_cli(["BRM_BACKUP_COMPLETE"],?SYS_LDN++BackupName, 5), 
    %% status=BRM_BACKUP_COMPLETE
     
    ok = rct_cli:disconnect(?CLI_Session),
    ok.

%%% ===========================================================================
%%% @doc
%%% Restore backup <br/>
%%% Using CLI to trig a restore backup . <br/>
%%% @spec restore_backup(_Config) -> ok
%%% @end
%%% ===========================================================================
restore_backup(_Config) ->
    
    %%Restore backup
    ok = rct_cli:connect(?CLI_Session),

    ResultInfo = lists:flatten(get_result_info(?BRM_LDN++" ,BrmBackupManager=1")),
    BackupName = lists:subtract(ResultInfo, "resultInfo="),
    %% Ex. resultInfo="BrM=1,BrmBackupManager=1,BrmBackup=6" 
    ct:log("Restore backup, BackupName: ~p~n",[BackupName]),

    LdnString = ?SYS_LDN++BackupName,
    rct_cli:send(?CLI_Session, LdnString),
    {ok, RecievedData} = rct_cli:send(?CLI_Session, "restore"),

    Data = string:tokens(lists:flatten(RecievedData), "\r\n "),
    %%ct:log("restore backup Data = ~p~n",[Data]),
    ReturnValue = lists:nth(2, Data),

    case decode_return_value(ReturnValue) of
    	actionStarted ->
    	    ok;
    	ErrorValue ->
    	    ct:log("Restore Backup error. Error: ~p~n",[ErrorValue]),
    	    ct:fail(ErrorValue)
    end,   

    ok = wait_for_exp_progress_state(["actionName=\"RESTORE\"", "result=SUCCESS", "state=FINISHED"], 
				     ?SYS_LDN++BackupName, 50), 
    
    ok = rct_cli:disconnect(?CLI_Session),

    timer:sleep(120000), %%to make sure cert is installed befor next TC runs
    ok.
%%% ===========================================================================
%%% @doc
%%%  <br/>
%%% Configure tls. <br/>
%%% @spec configure_tls(Config) ->  ok
%%% @end
%%% ===========================================================================
configure_tls(Config) -> 
  
    ct:pal("Checking/Configuring the site for TLS login"),

    Me_id = ?config(me_id, Config),
    Nc_id = ?config(nodeCredentialId, Config),
    Subject = ?config(user_ca_subject, Config),
    Tc_name = ?config(tc_name, Config),
    try find_ca(Me_id, Subject) of
	{Maybe_ca_id, "ENABLED", "VALID"}  when length(Maybe_ca_id) >= 1 ->
	    ct:pal("CA already present and ENABLED"),
	    ok;
	{Maybe_ca_id, _, _}  when length(Maybe_ca_id) >= 1 ->
						% a CA that works is installed
	    ct:pal("CA already present - activating"),
	    activate_ca(Me_id, Maybe_ca_id, Subject),
	    ct:pal("Waiting 5 sec to enable activation to go through"),
	    ok;
	What -> 
	    ct:pal("No CA found: ~p", [What]),
	    install_ca(Config, Subject)
    catch
	A:B ->
	    ct:pal("No CA found: ~p", [{A,B, erlang:get_stacktrace()}]),
	    install_ca(Config, Subject)
    end,
    {Ca_id, _, _} = find_ca(Me_id, Subject),
    case check_nc(Me_id, Nc_id) of
	ok -> 
	    ct:pal("NC already present"),
	    ok;
	nok ->
	    install_nc(Config)
    end,

    %%just create/recreate the TCat, no matter if already there (quick)
    ok = create_tc(Me_id, Tc_name, Ca_id),
    %%now we have everything needed in CERT (and LDAP is also done if SIM)
    case is_tls_login_active(Me_id, Nc_id, Tc_name) of
	ok ->
	    ct:pal("TLS already configured in SysM"),
	    ok;
	nok ->
	    activate_tls_login(Me_id, Nc_id, Tc_name),
	    ct:pal("Sleep 5 seconds after activation to give the server "
		   "time to start"),
	    timer:sleep(5000)
    end,

    ct:pal("TLS login configured - ready to run testcase"),
  
    ok.

%%% ===========================================================================
%%% @doc
%%% Test that it is possible to connect to CLI using TLS <br/>
%%% @spec connect_cli(Config) -> ok
%%% @end
%%% ===========================================================================
connect_cli(Config) ->   
    ct:pal("Running testcase connect_cli()", []),
    Host = ?config(dut_ip, Config),
    Port = ?config(cli_tls, Config),
    Certfile = ?config(expert_cert_file, Config),
    Keyfile = ?config(expert_key_file, Config),
    Opts = [{certfile, Certfile}, {keyfile, Keyfile},
	    {reuse_sessions, false}, %needed as we have this in server (BUG!)
	    {verify, verify_none},
	    {mode, binary}],

    %%ct:pal("Opts: ~p", [Opts]),

    flush_ssl(),
    {ok, Sock} =ssl:connect(Host, Port, Opts, 1000),
    Motd_part = <<"case of doubt shall seek advice from his/her manager.">>,
    ok = look_for_ssl(Sock, Motd_part),
    flush_ssl(),
    ok = ssl:send(Sock, <<"show\n">>),
    ok = look_for_ssl(Sock, <<"ManagedElement=">>),
    ok = ssl:send(Sock, <<"exit\n">>),
    ok = wait_for_ssl_exit(Sock),
    ok = ssl:close(Sock),
    flush_ssl(),
    ok.

%%% ===========================================================================
%%% @doc
%%% Test that it is possible to connect to netconf using TLS <br/>
%%% @spec connect_netconf(Config) ->  ok
%%% @end
%%% ===========================================================================
connect_netconf(Config) ->   
    ct:pal("Running testcase connect_netconf()", []),
    Host = ?config(dut_ip, Config),
    Port = ?config(netconf_tls, Config),
    Certfile = ?config(expert_cert_file, Config),
    Keyfile = ?config(expert_key_file, Config),
    Opts = [{certfile, Certfile}, {keyfile, Keyfile},
	    {reuse_sessions, false}, %needed as we have this in server (BUG!)
	    {verify, verify_none},
	    {mode, binary}],

    flush_ssl(),
    {ok, Sock} =ssl:connect(Host, Port, Opts, 1000),
    send_netconf_hello(Sock),
    {ok, Session_id} = receive_and_check_hello(Sock, <<>>),
    ct:pal("Received netconf hello message with session-id: ~p", [Session_id]),
    send_netconf_close(Sock, Session_id),
    ok = wait_for_ssl_exit(Sock),
    ok = ssl:close(Sock),
    flush_ssl(),
    ok.
%%% ===========================================================================
%%% @doc
%%%  <br/>
%%% Connect via COLI using tls  <br/>
%%% @spec connect_rcscoli(Config) -> ok
%%% @end
%%% ===========================================================================
connect_rcscoli(Config) ->   
   ct:pal("Running testcase connect_rcscoli()", []),
    Host = ?config(dut_ip, Config),
    Port = ?config(coli_tls, Config),
    Certfile = ?config(expert_cert_file, Config),
    Keyfile = ?config(expert_key_file, Config),
    Opts = [{certfile, Certfile}, {keyfile, Keyfile},
	    {reuse_sessions, false}, %needed as we have this in server (BUG!)
	    {verify, verify_none},
	    {mode, binary}],
    flush_ssl(),
    {ok, Sock} =ssl:connect(Host, Port, Opts, 1000),
    Motd_part = <<"case of doubt shall seek advice from his/her manager.">>,
    ok = look_for_ssl(Sock, Motd_part),
    flush_ssl(),
    ok = ssl:send(Sock, <<"/misc/info\n">>),
    ok = look_for_ssl(Sock, <<"User: expert">>),
    ok = ssl:send(Sock, <<"exit\n">>),
    ok = wait_for_ssl_exit(Sock),
    ok = ssl:close(Sock),
    flush_ssl(),
    
    %% If group_2 delete back up
    delete_backup(Config),

    ok.

%% --------------------------
%% Internal functions
%% --------------------------

%% ===========================================================================
%% @doc
%% Set attribute in MOM via CLI <br/>
%% @spec set_attribute(AttString, LdnString, Check)-> ok
%% @end
%% ===========================================================================
set_attribute(AttString, LdnString, check)->

    Options = [global,{capture, all, binary}],

    rct_cli:send(?CLI_Session,"configure"),
    rct_cli:send(?CLI_Session, LdnString),
    {ok, RecievedData} = rct_cli:send(?CLI_Session, AttString),

    case re:run(RecievedData, "ERROR", Options) of
	{match, [[_Value]]} ->
	    ct:log("Attribute configuration error = ~p~n",[RecievedData]),
	    ct:fail("Attribute configuration error");
	nomatch ->
	    ok
    end,

    {ok,RecievedData2} =  rct_cli:send(?CLI_Session,"commit"), 

    case re:run(RecievedData2, "ERROR", Options) of
	{match, [[_Value2]]} ->
	    ct:log("commit error = ~p~n",[RecievedData2]),
	    ct:fail("commit error");
	nomatch ->
	    ok
    end,

    rct_cli:send(?CLI_Session, "top");

set_attribute(AttString, LdnString, no_check)->

    rct_cli:send(?CLI_Session,"configure"),
    rct_cli:send(?CLI_Session, LdnString),
    {ok, _} = rct_cli:send(?CLI_Session, AttString),
    {ok, _} =  rct_cli:send(?CLI_Session,"commit"), 
    rct_cli:send(?CLI_Session, "top").

%%% ===========================================================================
%%% @doc
%%% Wait for expected backup progress stat via CLI <br/>
%%% @spec wait_for_exp_progress_state([], _Mo, _NoOfTries) -> ok
%%% @end
%%% ===========================================================================
wait_for_exp_progress_state([], _Mo, _NoOfTries) ->
    ok;

wait_for_exp_progress_state([WantedString | RestWantedList], Mo, NoOfTries) ->
  
    case rct_cli:send(?CLI_Session, "show verbose "++Mo, print) of
	{ok, RecievedData} ->
	    case check_data(WantedString, RecievedData) of
		ok ->
		    wait_for_exp_progress_state(RestWantedList, Mo, NoOfTries);
		nok ->
		    case NoOfTries of
			0 ->
			    nok;
			_Else ->
			    ct:pal("Did not find ~p, trying again in 3 sec", [WantedString]),
			    timer:sleep(3000),
			    wait_for_exp_progress_state([WantedString] ++ RestWantedList, Mo, NoOfTries -1)
		    end
	    end;
	_Error ->
	    ok = rct_cli:disconnect(?CLI_Session),
	    ok = poll_reboot(?CLI_Session),
	    wait_for_exp_progress_state([WantedString] ++ RestWantedList, Mo, NoOfTries -1)
    end.

poll_reboot(Cli_Session)->

    Timer = 15000,
    timer:sleep(Timer),
    ct:pal("Wait ~p s before checking if node is up",[Timer/1000]),
    Response = rct_cli:connect(Cli_Session),
    %%ct:pal("Response:  ~p", [Response]),
    case Response of
	ok ->  
	    ct:pal("Reboot done"),
	    ok;	
	_-> 
	    poll_reboot(Cli_Session)
    end.

%%% ===========================================================================
%%% @doc
%%% Get result info from progress report via CLI <br/>
%%% @spec get_result_info(Mo)-> ok
%%% @end
%%% ===========================================================================
get_result_info(Mo)->

    {ok, RecievedData} = rct_cli:send(?CLI_Session, "show verbose "++Mo, print),
    %%ct:log("get_result_info RecievedData = ~p~n",[RecievedData]),

    Data = string:tokens(RecievedData, "\""),
    Data2 = string:tokens(lists:flatten(Data), "\r\n "),
    %%ct:log("get_result_info  Data2 = ~p~n",[Data2]),
    get_result_info("resultInfo=", Data2).

get_result_info(Prefix, Data2)->
    get_result_info(Prefix, Data2, []).

get_result_info(_Prefix, [], Result) ->
    Result;

get_result_info(Prefix, [Element | RestElement], Result) ->
    case lists:prefix(Prefix, Element) of
	true -> 
	    get_result_info(Prefix, RestElement, Result ++ [Element]);
	false ->
	    get_result_info(Prefix, RestElement, Result)
    end.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------	
check_if_vc_board()->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
    Secure_board.

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
%%% Description: Check if recieve data contanins teh wanted data
%%%--------------------------------------------------------------------
check_data(WantedString, RecievedData) ->

    Options = [global, %% check all rows
               {capture, all, binary}],

    case re:run(RecievedData, WantedString, Options) of
	{match, [[_Value]]} ->
	    ok;
	nomatch ->
	    nok
    end.

%%%--------------------------------------------------------------------
%%% Description: Construct sftp
%%%--------------------------------------------------------------------
sftp_uri(Config) ->

    Host = proplists:get_value(sftp_host, Config),
    User = proplists:get_value(sftp_user, Config),
    %% Dir = proplists:get_value(sftp_root, Config),
    Dir = ?EXP_IMP_DIR, 
    NodeName = proplists:get_value(nodeName, Config),
    "sftp://"++User++"@"++Host++Dir++NodeName.


%%%--------------------------------------------------------------------
%%% Description: Wait for BackUp from Jenkins
%%%--------------------------------------------------------------------
wait_for_backup(Get_CMD)->
    TimeMilliSec = timer:hours(2),
    wait_for_backup(Get_CMD, TimeMilliSec).  %% 2 hours

wait_for_backup(_Get_CMD, Timeout) when Timeout < 0 ->
    ct:fail("Expected BackUP from Jenkins does not exist within time.");

wait_for_backup(Get_CMD, Timeout)->
    Return = os:cmd(Get_CMD),
    NewReturn = lists:flatten(string:tokens(Return, "\n")),
    ct:log("Return from wait_for_backup = ~p~n",[NewReturn]),
    case NewReturn of	
	"not exist" ->
	    ct:pal("No backup yet: ~p, trying again in 20 sec", [NewReturn]),
	    timer:sleep(20000),
	    wait_for_backup(Get_CMD, Timeout - 20000);
	Backup ->
	    Backup
    end.


%%% ===========================================================================
%%% @doc
%%% Check data from cli. <br/>
%%% @spec check_data_from_cli(AttributeList, Mo, NoOfTries) -> ok|nok
%%% @end
%%% ===========================================================================
check_data_from_cli(AttributeList, Mo, NoOfTries) ->
    {ok, RecievedData} = rct_cli:send(?CLI_Session, "show verbose "++Mo, print),
    check_data_from_cli(AttributeList, Mo, NoOfTries, RecievedData).

check_data_from_cli([], _Mo, _NoOfTries, _RecievedData) ->
    ok;


check_data_from_cli([WantedString | RestWantedList], Mo, NoOfTries, RecievedData) ->
    
    case check_data(WantedString, RecievedData) of
	ok ->
	    check_data_from_cli(RestWantedList, Mo, NoOfTries, RecievedData);
	nok ->
	    case NoOfTries of
		0 ->
		    nok;
		_Else ->
		    ct:pal("Did not find ~p, trying again in 1 sec", [WantedString]),
		    timer:sleep(1000),
		    check_data_from_cli([WantedString] ++ RestWantedList, Mo, NoOfTries -1)
	    end
    end.


%%% ===========================================================================
%%% @doc
%%% Delete backup after testrun. <br/>
%%% @spec delete_backup(Config)-> ok|nok
%%% @end
%%% ===========================================================================
delete_backup(Config)->

    %% Clean UP. Delete backup ++ remove exportPackageLabelPrefix
    %%[{name,group_2}] = proplists:get_value(tc_group_properties, Config),
    [{name, Group}] = proplists:get_value(tc_group_properties, Config),

    case Group of
	group_2 ->
	    ok = rct_cli:connect(?CLI_Session),
	    BackUpName = proplists:get_value(backupName, Config),
	    ct:log("Group: ~p, Clean up Delete backup: ~p",[Group, BackUpName]),
	    rct_cli:send(?CLI_Session, ?BRM_LDN++" ,BrmBackupManager=1"),
	    {ok, _RecievedData} = rct_cli:send(?CLI_Session, "deleteBackup --name "++BackUpName),
	    {ok, _} = set_attribute("no exportPackageLabelPrefix", ?BRM_LDN, no_check),

	    ok = rct_cli:disconnect(?CLI_Session);
	_Other ->
	    continue
    end.

%%--------------------------------------------------------------------
%% @doc
%% Add needed parameters to Config -proplist,
%% the majority of environment hardcoding is here.
%% @spec prepare_config(Config) -> New_config
%% @end
%%--------------------------------------------------------------------
prepare_config(Config) ->
    Target_dep =
	case os:getenv("SIM_OR_TARGET") of
	    "sim" ->
		Ports =
		    lists:map(
			fun(Type) ->
			    {Type, rct_rpc:call(rpc1, sysEnv, get_port_conf,
						[Type], 10000)}
			end, [cli_tls, coli_tls, netconf_tls]),
		[{dut_ip, "localhost"} | Ports];
	    _ ->
		% default portnumbers on target (in SYS:make_release.escript)
		Dut = ?config(1, ct:get_config(test_nodes)),
		Dut_props =  ct:get_config(Dut),
		Dut_ip = ?config(ssh, ?config(ssh_lmt_ipv4, Dut_props)),
		[{dut_ip, Dut_ip}, {cli_tls, 9830}, {coli_tls, 9831},
		 {netconf_tls, 6513}]
	end,
    My_priv_dir =
	filename:join(
	    lists:droplast(
		filename:split(
		    ?config(priv_dir, Config)))),
    Nc_file_name = "tls_nodecert_pkcs12",
    User_ca_file_name = "user-ca.crt",
    Me_id = "1", 

    [{me_id, Me_id},
     {my_priv_dir, My_priv_dir},
     {expert_cert_file,
      filename:join([?config(data_dir, Config), "user_expert.crt"])},
     {expert_key_file,
      filename:join([?config(data_dir, Config), "user_expert.key"])},
     {alt_expert_cert_file,
      filename:join([?config(data_dir, Config), "alt_user_expert.crt"])},
     {alt_expert_key_file,
      filename:join([?config(data_dir, Config), "alt_user_expert.key"])},
     {nodeCredentialId, "TLS_login"},
     {nc_subject_name, "esekilvxen519.rnd.ki.sw.ericsson.se"},
     {nc_key_info, "RSA_2048"},
     {nc_finger_p,
      "17:8b:19:ef:57:e1:12:62:67:33:f5:bd:bd:8c:28:8e:bd:4b:c2:ce"},
     {nc_cred_pwd, "idiocy"},
     {nc_file, Nc_file_name},
     {tc_name, "TLS_login_tc"},
     {user_ca_file , User_ca_file_name},
     {user_ca_subject,
	"C=se,ST=Stockholm,L=Kista,O=RBS-CS,OU=etxlg's User CA,CN=User CA"},
     {user_ca_finger_p,
      "DB:59:94:FB:BE:E6:1B:83:D4:77:88:BF:F8:27:9B:B9:BC:A0:5D:23"},
     {cert_to_sftp_files,[Nc_file_name, User_ca_file_name]},
     {sftp_server, ct:get_config(sftp_server)},
     {ldap_server, ?config(host, ct:get_config(ldap_server))},
     {ldap_base_dn, "ou=people,dc=mordor,dc=invalid"},
     {ldap_bind_dn, "cn=king,dc=mordor,dc=invalid"},
     {ldap_bind_pw, "1:7TcvZCTcqkKUI6RNL3IKSlMB/kas"} |
	Target_dep] ++ Config.

%%% ===========================================================================
%%% @doc
%%% Prepare files for tls config <br/>
%%% @spec prepare_files(Config) -> ok
%%% @end
%%% ===========================================================================
prepare_files(Config) ->
    % copy the files that the node may need to fetch using sftp
    % tls_nodecert_pkcs12: used as nodecredential in the TLS server
    % user-ca.crt: CA, (trusted-certificate) used to validate the login-cert
    Files = ?config(cert_to_sftp_files, Config),
    Priv_dir = ?config(my_priv_dir, Config),
    Data_dir = ?config(data_dir, Config),
    ct:pal("prepare_files: ~p, ~p, ~p", [Files, Priv_dir, Data_dir]),
    [begin
        Fpriv = filename:join([Priv_dir, F]),
        Fdata = filename:join([Data_dir, F]),
        case {file:read_file(Fpriv), file:read_file(Fdata)} of
            {{ok, Same}, {ok, Same}} -> ok;
            {_, {ok, Bin_file}} ->
                ok = file:write_file(Fpriv, Bin_file)
        end
     end || F <- Files].


%%%--------------------------------------------------------------------
%%% Description: functions for configure tls
%%%--------------------------------------------------------------------
%return a tuple ex:
%{"1", "ENABLED", "VALID"} | {"1", "DISABLED", "NOT_VALID_YET"}
find_ca(Me_id, Wanted_subject) ->
    Get =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[Me_id]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
             [{secMId,[],["1"]},
              {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
               [{certMId,[],["1"]}]}]}]}]},
    {ok, Conf} = netconf(get, [nc1, Get]),
    Names = ['ManagedElement','SystemFunctions','SecM','CertM',
             'TrustedCertificate'],
    All_trusted = struct(Conf, Names),
    Id_sub_valid_and_state =
        [begin
            [Id] = struct(T, [trustedCertificateId]),
            [Valid] = struct(T, [managedState]),
            [State] = struct(T, [certificateState]),
            [Cert_cont] = struct(T, [certificateContent]),
            [Subject] = struct(Cert_cont, [subject]),
            {Id, Subject, Valid, State}
        end || T <- All_trusted],
    %if there are several just chose the first
    Found_ids =
	[begin 
	    ct:pal(
		"Matching CA, Id: ~p, managedState: ~p, certificateState: ~p",
		[Id, Valid, State]),
	    {Id, Valid, State}
	end  || {[Id], Subject, [Valid], [State]} <- Id_sub_valid_and_state,
                    Subject =:= [Wanted_subject]],
    ct:pal("Matching CA ids: ~p", [Found_ids]),
    case Found_ids of %jenkins console parser doesn't like crashes/errors
	[] -> {nok, "no matching CA installed"};
	_ ->  hd(Found_ids)
    end.

activate_ca("1", Ca_id, Subject) ->
    activate_ca("1", Ca_id, Subject, 1).

activate_ca(_, _, _, 11) ->
    ct:fail("Unable to activate the CA");

activate_ca(Me_id, Ca_id, Subject, N) ->
    ct:pal("Activating CA with ID: ~p, try: ~p of 10", [Ca_id, N]),
    Edit_enable = get_activate_ca_edit(Me_id, Ca_id, "ENABLED"),
    Edit_disable = get_activate_ca_edit(Me_id, Ca_id, "DISABLED"),
    ok = netconf(edit_config, [nc1, running, Edit_enable]),
    case find_ca(Me_id, Subject) of
	{_Id, "ENABLED", "VALID"} ->
	    ok;
	{_Id, Manstate, Certstate} ->
	    ct:pal("CERT workaround, managedState: ~p, certificateState: ~p~n"
		   "DISABLING CA", [Manstate, Certstate]),
	    ok = netconf(edit_config, [nc1, running, Edit_disable]),
	    ct:pal("CERT workaround, ENABLING again and waiting 1 second"),
	    ok = netconf(edit_config, [nc1, running, Edit_enable]),
	    timer:sleep(1000),
	    activate_ca(Me_id, Ca_id, Subject, N + 1)
    end.

get_activate_ca_edit(Me_id, Ca_id, Enable_disable) ->
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[Me_id]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
             [{secMId,[],["1"]},
              {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
               [{certMId,[],["1"]},
                {'TrustedCertificate',
                 [{trustedCertificateId,[],[Ca_id]},
                  {managedState,[],[Enable_disable]}]}]}]}]}]}.


wait_for_ca(_, Subject, 0) ->
    ct:fail("Timeout waiting for CA creation: ~p", [Subject]);
wait_for_ca(Me_id, Subject, N) ->
    try find_ca(Me_id, Subject) of
	{Ca_id, _, _}  when length(Ca_id) >= 1 ->
	    Ca_id;
	_ -> 
	    timer:sleep(1000),
	    wait_for_ca(Me_id, Subject, N - 1)
    catch
	_:_ ->
	    timer:sleep(1000),
	    wait_for_ca(Me_id, Subject, N - 1)
    end.

install_ca(Config, Subject) ->
    Me_id = ?config(me_id, Config),
    Subject = ?config(user_ca_subject, Config),
    Dir = ?config(my_priv_dir, Config),
    Sftp_data = ?config(sftp_server, Config),
    Shost = ?config(host, Sftp_data),
    Uname = ?config(username, Sftp_data),
    Spwd = ?config(password, Sftp_data),
    File =  filename:join([Dir, ?config(user_ca_file, Config)]),
    Uri = "sftp://" ++ Uname ++ "@" ++ Shost ++ File,
    Finger_p = ?config(user_ca_finger_p, Config),

    Action =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[Me_id]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
             [{secMId,[],["1"]},
              {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
               [{certMId,[],["1"]},
                {installTrustedCertFromUri, [],
                 [{uri, [Uri]},
                  {uriPassword, [Spwd]},
                  {fingerprint, [Finger_p]}]}]}]}]}]},
    ct:pal("Install CA for TLS-login",[]),
    case  netconf(action, [nc1, Action]) of
        {ok, _} ->
	    Ca_id = wait_for_ca(Me_id, Subject, 10),
	    ct:pal("CA was found: ~p, - waiting 10 seconds to see if CERT "
		   "allows activation (remove this when not needed).", [Ca_id]),
	    timer:sleep(10 * 1000),
	    activate_ca(Me_id, Ca_id, Subject);
        Error ->
            ct:fail(Error)
    end.

install_nc(Config) ->
    Nc_name = ?config(nodeCredentialId, Config),
    Subject_name = ?config(nc_subject_name, Config),
    Key_info = ?config(nc_key_info, Config),
    Finger_p = ?config(nc_finger_p, Config),
    Cred_pwd = ?config(nc_cred_pwd, Config),
    Me_id = ?config(me_id, Config),
    Dir = ?config(my_priv_dir, Config),
    Sftp_data = ?config(sftp_server, Config),
    Shost = ?config(host, Sftp_data),
    Uname = ?config(username, Sftp_data),
    Spwd = ?config(password, Sftp_data),
    File =  filename:join([Dir, ?config(nc_file, Config)]),
    Uri = "sftp://" ++ Uname ++ "@" ++ Shost ++ File,

    ok = prepare_nc(Me_id, Subject_name, Key_info, Nc_name),
    ok = install_nc(Me_id, Uri, Spwd, Cred_pwd, Finger_p, Nc_name),
    wait_for_nc(Me_id, Nc_name, 10).

wait_for_nc(_, Nc_name, 0) -> 
    ct:fail("Timeout waiting for NC creation: ~p", [Nc_name]);
wait_for_nc(Me_id, Nc_name, N) -> 
    case check_nc(Me_id, Nc_name) of
	ok -> ok;
	nok ->
	    ct:pal("NC not yet imported, wait another second: ~p", [N]),
	    timer:sleep(1000),
	    wait_for_nc(Me_id, Nc_name, N - 1)
    end.

install_nc(Me_id, Uri, Pwd, Cred_pwd, Finger_print, Nc_name) ->
    ct:pal("Uri: ~p~n", [Uri]),
    Install = {'ManagedElement',
              [{xmlns,"urn:com:ericsson:ecim:ComTop"},
               {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
              [{managedElementId,[],[Me_id]},
               {'SystemFunctions',
                [{systemFunctionsId,[],["1"]},
                 {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                  [{secMId,[],["1"]},
                   {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                    [{certMId,[],["1"]},
                     {'NodeCredential',
                      [{nodeCredentialId, [Nc_name]},
                       {installCredentialFromUri,
                        [{uri, [Uri]},
                         {uriPassword, [Pwd]},
                         {credentialPassword, [Cred_pwd]},
                         {fingerprint, [Finger_print]}]}]}]}]}]}]},

    ct:pal("Action install TLS NC",[]),
    case  netconf(action, [nc1, Install]) of
        {ok, _} ->
            ok;
        Error ->
            ct:fail(Error)
    end.

prepare_nc(Me_id, Subject_name, Key_info, Nc_name) ->
    Edit = {'ManagedElement',
            [{xmlns,"urn:com:ericsson:ecim:ComTop"},
             {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
            [{managedElementId,[],[Me_id]},
             {'SystemFunctions',
              [{systemFunctionsId,[],["1"]},
               {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                [{secMId,[],["1"]},
                 {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                  [{certMId,[],["1"]},
                   {'NodeCredential',
                    [{nodeCredentialId, [Nc_name]},
                     {userLabel, ["Created by " ++ atom_to_list(?MODULE)]},
                     {subjectName, [Subject_name]},
                     {keyInfo, [Key_info]}]}]}]}]}]},

    ct:pal("Create TLS Node Credential",[]),
    case netconf(edit_config, [nc1, running, Edit]) of
        ok -> ok;
        Error1 ->
            ct:fail(Error1)
    end.

check_nc(Me_id, Nc) ->
    Get =
        {'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"},
	  {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
	 [{managedElementId,[],[Me_id]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
	     [{secMId,[],["1"]},
	      {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
	       [{certMId,[],["1"]},
		{'NodeCredential',
		 [{nodeCredentialId, [Nc]}]}]}]}]}]},
    Names = ['ManagedElement','SystemFunctions','SecM','CertM',
	     'NodeCredential', 'certificateState'],
    try
        begin
            {ok, Conf} = netconf(get, [nc1, Get]),
            struct(Conf, Names)
        end of
        [["VALID"]] ->
            ok;
        Bad ->
            ct:pal("check_nc, bad -> nok: ~p", [Bad]),
            nok
    catch
        A:B ->
            ct:pal("check_nc, catch  -> nok: ~s", [make_safe({A, B})]),
            nok
    end.


create_tc(Me_id, Tc_name, Ca_id) ->
    Mo_ref = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
             "TrustedCertificate=" ++ Ca_id,
    Edit =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[Me_id]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
             [{secMId,[],["1"]},
              {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
               [{certMId,[],["1"]},
                   {'TrustCategory', [],
                    [{trustCategoryId, [], [Tc_name]},
                     {userLabel, ["Created by " ++ atom_to_list(?MODULE)]},
                     {trustedCertificates, [Mo_ref]}]}]}]}]}]},

    ct:pal("Creating TC, Id: ~p~nContaining: ~p", [Tc_name, Mo_ref]),
    ok = netconf(edit_config, [nc1, running, Edit]).


is_tls_login_active(Me_id, Nc_name, Tc_name) ->
    case {is_tls_login_active(netconf, Me_id, Nc_name, Tc_name), 
	  is_tls_login_active(cli, Me_id, Nc_name, Tc_name)} of
	{ok, ok}  ->
	    ok;
	{nok, nok}  ->
	    nok;
	{_, _}  ->
	    ct:pal("WARNING cli/netconf partly enabled/disabled", []),
	    nok
    end.

is_tls_login_active(Service, Me_id, Nc_name, Tc_name) ->
    Service_atom = service_atom(Service),
    Service_id = service_id(Service),
    Nc_dn = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
              "NodeCredential=" ++ Nc_name,
    Tc_dn = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
              "TrustCategory=" ++ Tc_name,
    Get =
        {'ManagedElement',
        [{xmlns,"urn:com:ericsson:ecim:ComTop"},
         {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
        [{managedElementId,[],[Me_id]},
         {'SystemFunctions',
          [{systemFunctionsId,[],["1"]},
           {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
            [{sysMId,[],["1"]},
             {Service_atom, [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
              [{Service_id,[],["1"]}]}]}]}]},
    {ok, Conf} = netconf(get, [nc1, Get]),

    Tls_base = ['ManagedElement','SystemFunctions','SysM', Service_atom],

    case {struct(Conf, Tls_base ++ ['nodeCredential']),
          struct(Conf, Tls_base ++ ['trustCategory']),
          struct(Conf, Tls_base ++ ['administrativeState'])} of
        {[[Nc_dn]], [[Tc_dn]], [["UNLOCKED"]]} -> ok;
        _ -> nok
    end.


activate_tls_login(Me_id, Nc_name, Tc_name) ->
    [activate_tls_login(Service, Me_id, Nc_name, Tc_name) ||
	Service <- [netconf, cli]].

activate_tls_login(Service, Me_id, Nc_name, Tc_name) ->
    Service_atom = service_atom(Service),
    Service_id = service_id(Service),
    Nc_dn = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
              "NodeCredential=" ++ Nc_name,
    Tc_dn = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
              "TrustCategory=" ++ Tc_name,
    Edit =
        {'ManagedElement',
        [{xmlns,"urn:com:ericsson:ecim:ComTop"},
         {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
        [{managedElementId,[],[Me_id]},
         {'SystemFunctions',
          [{systemFunctionsId,[],["1"]},
           {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
            [{sysMId,[],["1"]},
             {Service_atom, [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
              [{Service_id,[],["1"]},
               {administrativeState, [], ["UNLOCKED"]},
               {nodeCredential, [], [Nc_dn]},
               {trustCategory, [], [Tc_dn]}]}]}]}]},
    ct:pal("Activate TLS: ~p: login",[Service]),
    case netconf(edit_config, [nc1, running, Edit]) of
        ok -> ok;
        Error1 ->
            ct:fail(Error1)
    end.

service_atom(netconf) -> 'NetconfTls';
service_atom(cli) -> 'CliTls'.

service_id(netconf) -> 'netconfTlsId';
service_id(cli) -> 'cliTlsId'.

netconf(F, A) ->
    %%{ok, _} = ct_netconfc:open(nc1, [{user, "expert"}, {password, "expert"}]),
    {ok, _} = ct_netconfc:open(nc1, []),
    Res = apply(ct_netconfc, F, A),
    case Res of
        {error, _} ->
	    catch ct_netconfc:close_session(nc1),
            Res;
        _ ->
            ok = ct_netconfc:close_session(nc1)
    end,
    Res.

%descend into the structure following list(Names), when only one Name
%remains collect all matching and return them
struct([{Name, _, Next} | T], [Name]) ->
    [Next | struct(T, [Name])];
struct([{_, _, _} | T], [Name]) ->
    struct(T, [Name]);
struct([], _) -> [];
struct([{Name, _, Next} | _], [Name | Names]) ->
    struct(Next, Names);
struct([{_, _, _} | T], Names) ->
    struct(T, Names).


make_safe({A, B}) ->
    "{" ++ m_safe(A) ++ "," ++ m_safe(B) ++ "}".

m_safe(C) ->
    re:replace(
	io_lib:format("~p", [C]),
	"error", "boogaloo",
	[global, {return, list}]).

send_netconf_hello(Sock) ->
    Hello =
	<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
	  "<hello xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">"
	  "<capabilities>"
	  "<capability>urn:ietf:params:netconf:base:1.0</capability>"
	  "</capabilities>"
	  "hello> ]]>]]>">>,
    ssl:send(Sock, Hello).

send_netconf_close(Sock, Session_id) ->
    Close =
        <<"<rpc message-id=\"",
           (integer_to_binary(Session_id))/binary,
           "\" xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">"
          "<close-session></close-session> </rpc>]]>]]>">>,
    ssl:send(Sock, Close).

receive_and_check_hello(Sock, Acc)->
    receive
        {ssl, Sock, Bin} ->
            New_acc = <<Acc/binary, Bin/binary>>,
            case check_hello(New_acc) of
                {ok, _} = Good ->
                    Good;
                nok ->
                    receive_and_check_hello(Sock, New_acc)
            end;
        Other ->
            ct:pal("receive_and_check_hello(...): got other: ~p~n", [Other]),
            nok
after
    10000 ->
        ct:pal("receive_and_check_hello/2, timeout: ~p~n", [Acc]),
        nok
end.

check_hello(Hello) ->
    case binary:split(Hello, <<"]]>]]>">>) of
        [H,_] ->
            xml_check_hello(xmerl_scan:string(binary_to_list(H)));
        _ ->
            nok
    end.

xml_check_hello({#xmlElement{name = hello} = E, _}) ->
    xml_check_hello(E#xmlElement.content);
xml_check_hello([#xmlElement{name = 'session-id', content = [C]} | _]) ->
    {ok, list_to_integer(C#xmlText.value)};
xml_check_hello([_ | T]) ->
    xml_check_hello(T);
xml_check_hello(_) ->
    nok.


wait_for_ssl_exit(Sock) ->
    receive
        {ssl_closed, Sock} ->
            ct:pal("SSL connection closed", []),
            ok;
        _ ->
            wait_for_ssl_exit(Sock)
    after
        10000 ->
        ct:pal("wait_for_ssl_exit/1, timeout waiting for ssl to close", []),
        nok
    end.

look_for_ssl(Sock, Find) ->
    look_for_ssl(Sock, Find, <<>>).
look_for_ssl(Sock, Find, Acc)->
    receive
        {ssl, Sock, Bin} ->
            New_acc = <<Acc/binary, Bin/binary>>,
            case binary:match(New_acc, Find) of
                nomatch ->
                    look_for_ssl(Sock, Find, New_acc);
                _ ->
                    ct:pal("Matched: ~p", [Find]),
                    ok
            end;
        Other ->
            ct:pal("look_for_ssl(...): got other: ~p~n", [Other]),
            look_for_ssl(Sock, Find, Acc)
after
    10000 ->
        ct:pal("look_for_ssl/3, timeout: ~p~n", [Acc]),
        nok
end.

flush_ssl() ->
    receive
        {ssl, _, _}->
            flush_ssl()
    after
        0 -> ok
    end.

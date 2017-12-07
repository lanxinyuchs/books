%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sys_fi_SUITE.erl %
%%% @author ejinfeg
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/4
%%% 
%%% @doc ==Tests of the File interface==
%%% @end

-module(sys_fi_SUITE).

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
%%% Rev        Date        Name        What
%%% -----      ----------  --------    -----------------------
%%% R3A/1      2014-09-11  etxpejn     Created
%%% R3A/2      2014-09-15  etxpejn     Added checks for validations.
%%% R3A/6      2015-01-08  erarafo     Added test_write/1, work in progress
%%% R3A/7      2015-01-09  erarafo     Refactoring
%%% R3A/8      2015-01-12  erarafo     Test scenario elaborated
%%% R3A/9      2015-01-15  erarafo     Timing adjusted
%%% R3A/10     2015-01-16  erarafo     Verification by SFTP added
%%% R3A/11     2015-01-21  erarafo     Setting parameters by COLI
%%% R3A/12     2015-01-22  erarafo     Speedup
%%% R3A/13     2015-01-27  erarafo     Added tests for persistent files
%%% R3A/17     2015-02-23  eolaand     reclaimFiles disabled
%%% R3A/18     2015-02-28  etxkols     Preparation for 2 labs
%%% R3A/2      2015-07-10  etxjovp     Add group definitions used by CS CI
%%% R4A/2      2015-09-28  etxpejn     Removed etxpejn dir
%%% R4A/4      2015-10-14  etxpejn     Added dual groups
%%% R4A/5      2015-11-12  eolaand     Temp removal of TC
%%% R4A/6      2015-11-12  eolaand     Add 2 removed TC:s again
%%% R5A/1      2015-11-30  etxpejn     Made export_ and import_file more robust
%%% R5A/2      2016-05-20  etxpejn     Prolonged timer to call ssh_sftp
%%% R6A/1      2016-09-06  etxkols     GIT and cloud fixes
%%% R7A/1      2016-09-06  etxpejn     Changed random to rand
%%% R7A/2      2016-yy-xx  etxpejn     Added tc export_file_encrypt and import_file_encrypt
%%% R8A/1      2016-12-12  etxkols     Removing rs232 hook for cloud
%%% R8A/1      2016-12-12  etxpejn     Added test for faulty fullPath
%%% R8A/2      2016-12-29  etxpejn     Temp removed test for faulty fullPath
%%% R9A/3      2017-04-03  estjako     ftpes_group added in all()
%%% R9A/4      2017-04-04  estjako     Removed ftpes_group from all()- fails on 5G environment
%%% R10A/1     2017-06-02  ejinfeg     Add new test case "import_file3"
%%% R10A/2     2017-06-14  etxpejn     Temp removed import_file3 from ftpes_group
%%% R10A/3     2017-06-29  ejinfeg     Added back import_file3 to ftpes_group
%%% ----------------------------------------------------------

-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 export_file/1,
	 export_file_encrypt/1,
	 import_file/1,
	 import_file_encrypt/1,
	 import_file3/1,
	 test_writeRamFiles/1,
	 test_writeSsdFiles/1,
	 groups/0
	]).

-define(FI, 17).

-define(FiImportFile, 0).
-define(FiExportFile, 1).
-define(FiOpenWrite, 2).
-define(FiImportFile2, 3).
-define(FiExportFile2, 4).
-define(FiImportFile3, 5).

-define(FI_OK, 0).
-define(FI_INVALID_URI, 1).
-define(FI_FAILED_TO_CONNECT_TO_SFTP_SERVER, 2).
-define(FI_FAILED_TO_IMPORT_OR_EXPORT_FILE, 3).
-define(FI_FAILED_TO_OPEN_FILE, 6).
-define(FI_FAILED_TO_OBTAIN_DIRECTORY, 7).
-define(FI_SPACE_FULL, 8).
-define(FI_FAILED_TO_DECRYPT_PASSWORD, 17).
-define(FI_FILE_SIZE_EXCEED_LIMITATION, 18).
-define(FI_NO_ENOUGH_DISK_SPACE, 19).
-define(FTPES_SERVER_SYS_DIR, "SYS").



-define(TestDir, rct_cc_git_path:find("RCS_TOP", ["SYS/SYS_CNX9012620/test/suites/", "SYS/test/suites/"])).
-define(ProjDir, "/proj/rcs-tmp/stps/").
-define(FiFile, "fi_valmanifest.txt").

-define(TEST_FILE_SIZE, 10000).

-define(RAM_TEST_DIR, "eventRop").

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    Serial = case os:getenv("SIM_OR_TARGET") of
         "cloudish" -> [];
         _ -> [{rct_rs232, {console,[{connect_retries, 3}]}}]
         end,
    [{ct_hooks, [{rct_sftp_client, []},
         {rct_htmllink,[]},
         {rct_rpc, rpc},
         {rct_ssh, ssh},
         {rct_netconf,{nc1, html}},                 
         {rct_coli, {coli, [manual_connect]}},
         {rct_proxy,[{1, node1, ssh_lmt_ipv4, du1, username}]},
                 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
                           ["sysFi: Failed to"]}}]}},
                ftpes_hook()
        ] ++ Serial}].


%% @hidden
%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = [tuple()]
%% Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    [{ftp_protocol, sftp}|Config].

%% @hidden
%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = [tuple()]
%% Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

init_per_group(ftpes_group, Config) ->
    ok = rct_ftpes_client:open(),
    Folder = ftpes_test_lib:create_test_folder(?FTPES_SERVER_SYS_DIR),
    NewConfig = lists:keyreplace(ftp_protocol, 1, Config, {ftp_protocol, ftpes}),
    
    %% Create NodeCredential and activate ftpes feature
    Started = ftpes_test_lib:start_server(rpc),
    NewConfig2 = ftpes_test_lib:initialize_nc_tc(NewConfig),
    ftpes_test_lib:enable_ftpes_tls(NewConfig2),
    
    [{started, Started}, {test_folder, Folder} | NewConfig2];

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(ftpes_group, Config) ->    
    ftpes_test_lib:disable_ftpes_tls(),
    ok = rct_ftpes_client:cd(".."),
    ok = rct_ftpes_client:rmdir(?config(test_folder, Config)),
    case ?config(started, Config) of
        no -> ftpes_test_lib:stop_server();
        yes -> ok
    end,
    ftpes_test_lib:clean_nc_tc(Config),
    
    ok = rct_ftpes_client:close();

end_per_group(GroupName, _Config) when GroupName =:= fi_test ->
    %% ok = rct_proxy:exit_master(node1),
    ok;
end_per_group(_GroupName, _Config) ->
    ok.


%% @hidden
%%--------------------------------------------------------------------
%% @doc
%% Start Client and add it in Config.<br/>
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = [tuple()]
%% Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(export_file, Config) ->
    Proto = ?config(ftp_protocol, Config),
    {ok, client_started} = rct_proxy:start_proxy(node1, fi1, ?FI),
    NewConfig = get_params(Config),
    SysPath = ?config(sys_path, NewConfig),   
    Uri = ?config(uri_import, NewConfig),
    Password = ?config(password, NewConfig),
    
    case Proto of
        sftp ->  {ok,_} = file:copy(?TestDir++?FiFile, SysPath++"/"++?FiFile);
        ftpes -> {ok, Binary} = file:read_file(?TestDir++?FiFile),
                 ok = rct_ftpes_client:write_file(SysPath++"/"++?FiFile, Binary)
    end,
    
    ct:pal("File transfer import - ok"),
    {ok, ?FI_OK} = 
        rct_proxy:send_proxy(node1, fi1, ?FiImportFile,
                     {Password, 
                      Uri ++ ?FiFile,
                     "applicationlogs/"}),
    ok = check_result_on_node(os:getenv("SIM_OR_TARGET"), "/applicationlogs/"++?FiFile),
    NewConfig;

init_per_testcase(export_file_encrypt, Config) ->
    Proto = ?config(ftp_protocol, Config),
    {ok, client_started} = rct_proxy:start_proxy(node1, fi1, ?FI),
    NewConfig = get_params(Config),
    SysPath = ?config(sys_path, NewConfig),   
    Uri = ?config(uri_import, NewConfig),
    Password = ?config(password, NewConfig),
    
    case Proto of
        sftp ->  {ok,_} = file:copy(?TestDir++?FiFile, SysPath++"/"++?FiFile);
        ftpes -> {ok, Binary} = file:read_file(?TestDir++?FiFile),
                 ok = rct_ftpes_client:write_file(SysPath++"/"++?FiFile, Binary)
    end,

    EncryptPassword = rct_rpc:call(rpc, comsaI, encrypt_password, [list_to_binary(Password)], 5000),

    ct:pal("File transfer import - ok"),
    {ok, ?FI_OK} = 
        rct_proxy:send_proxy(node1, fi1, ?FiImportFile2,
                     {EncryptPassword, 
                      Uri ++ ?FiFile,
                  "applicationlogs/"}),
    ok = check_result_on_node(os:getenv("SIM_OR_TARGET"), "/applicationlogs/"++?FiFile),
    NewConfig;
    
init_per_testcase(_TestCase, Config) ->
    {ok, client_started} = rct_proxy:start_proxy(node1, fi1, ?FI),
    NewConfig = get_params(Config),
    NewConfig.
    
get_params(Config) ->
    FtpProtocol = ?config(ftp_protocol, Config),
    [{host, Host},{username, User},{password, Password}] = 
        get_ftp_config(FtpProtocol),
    ImportUri = get_ftp_uri_import(FtpProtocol, User, Host),
    ExportUri = get_ftp_uri_export(FtpProtocol, User, Host),
    SysPath = get_sys_path(FtpProtocol),
    [{uri_import, ImportUri}, {uri_export, ExportUri}, {password, Password},
      {sys_path, SysPath}, {host, Host}, {username, User} | Config].


%% @hidden
%%--------------------------------------------------------------------
%% @doc
%% Stop the Client that was started from init_  .<br/>
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = [tuple()]
%% Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(import_file, Config) ->
    JenkinsNode = lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
    Proto = ?config(ftp_protocol, Config),
    ok = check_result_on_server(Proto,
                                case Proto of
                                    sftp -> ?ProjDir++JenkinsNode ++ "/" ++ ?FiFile;
                                   ftpes -> ?config(sys_path, Config)++ "/"++?FiFile
                                end, 
                                Config),
    {ok, client_stopped} = rct_proxy:stop_proxy(node1, fi1),
    ok;

end_per_testcase(import_file_encrypt, Config) ->
    JenkinsNode = lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
    Proto = ?config(ftp_protocol, Config),
    ok = check_result_on_server(Proto,
                            case Proto of
                                sftp -> ?ProjDir++JenkinsNode ++ "/" ++ ?FiFile;
                               ftpes -> ?config(sys_path, Config)++ "/"++?FiFile
                            end, 
                            Config),
    {ok, client_stopped} = rct_proxy:stop_proxy(node1, fi1),
    ok;

end_per_testcase(import_file3, Config) ->
    JenkinsNode = lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
    Proto = ?config(ftp_protocol, Config),
    ok = check_result_on_server(Proto,
                            case Proto of
                                sftp -> ?ProjDir++JenkinsNode ++ "/" ++ ?FiFile;
                               ftpes -> ?config(sys_path, Config)++ "/"++?FiFile
                            end, 
                            Config),
    {ok, client_stopped} = rct_proxy:stop_proxy(node1, fi1),
    ok;

end_per_testcase(_TestCase, _Config) ->
    {ok, client_stopped} = rct_proxy:stop_proxy(node1, fi1),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [{group, fi_test},
     {group, ramfiles},
     {group, ssdfiles}
    ].

groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], []},  
     {sbc__upgrade__all__1__group, [], []},  
     {sdc__cover__sim__1__group, [], []},
     {sdc__def__all__1__group, [], []},  
     {sdc__qual__all__1__group, [], []}, 
     {fi_test, [sequence],
     [
      import_file,
      import_file_encrypt,
      import_file3,
      export_file,
      export_file_encrypt
     ]},
     {ramfiles, [sequence],
      [
       test_writeRamFiles
      ]},
     {ssdfiles, [sequence],
      [
       test_writeSsdFiles
      ]},
     {ftpes_group, [sequence],
      [
      import_file,
      export_file,
      import_file_encrypt,
      import_file3,
      export_file_encrypt
      ]},
     %% This suite should only be run on core MP within a sim cluster
     {sbc__cluster__dual_sim_1__1__group, [], [{group, default__group}]}
    ].


%%--------------------------------------------------------------------
%% @doc 
%% 
%% @spec import_file(Config) -> ok
%% @end
%%--------------------------------------------------------------------
import_file(Config) ->
    SysPath = ?config(sys_path, Config),   
    Uri = ?config(uri_import, Config),
    Password = ?config(password, Config),
    Proto = ?config(ftp_protocol, Config),
    Host = ?config(host, Config),
    
    case Proto of
        sftp ->  {ok,_} = file:copy(?TestDir++?FiFile, SysPath++"/"++?FiFile);
        ftpes -> {ok, Binary} = file:read_file(?TestDir++?FiFile),
                 ok = rct_ftpes_client:write_file(SysPath++"/"++?FiFile, Binary)
    end,
    
    ct:pal("File transfer import - ok"),
    {ok, ?FI_OK} = 
        rct_proxy:send_proxy(node1, fi1, ?FiImportFile,
                     {Password, 
                      Uri ++ ?FiFile,
                     "applicationlogs/"}),
    ok = check_result_on_node(os:getenv("SIM_OR_TARGET"), "/applicationlogs/"++?FiFile),

    %% ct:pal("File transfer import with faulty fullpath"),
    %% {ok, ?FI_FAILED_TO_IMPORT_OR_EXPORT_FILE} = 
    %%  rct_proxy:send_proxy(node1, fi1, ?FiImportFile,
    %%               {Password, 
    %%                "sftp://"++User++"@"++Sftp++?ProjDir++JenkinsNode++"/"++?FiFile,
    %%                "/applicationlogs/"}),
    
    ct:pal("File transfer import with faulty URI"),
    {ok, ?FI_INVALID_URI} = 
        rct_proxy:send_proxy(node1, fi1, ?FiImportFile,
                     {Password, 
                    "//proj/rcs-tmp/"++?FiFile,
                      ?FiFile}),

    ct:pal("File transfer import with wrong password"),
    {ok, ?FI_FAILED_TO_CONNECT_TO_SFTP_SERVER} = 
        rct_proxy:send_proxy(node1, fi1, ?FiImportFile,
                     {"wrong_password", 
                      Uri ++ ?FiFile,
                  ?FiFile}),

    case Proto of 
        sftp -> ct:pal("File transfer import with wrong user"),
                {ok, ?FI_FAILED_TO_CONNECT_TO_SFTP_SERVER} = 
                    rct_proxy:send_proxy(node1, fi1, ?FiImportFile,
                                 {Password, 
                                  "sftp://"++ "wronguser" ++"@"++Host ++ "/" ++ ?FiFile,
                                  ?FiFile});
        ftpes -> ok
    end,

    ct:pal("File transfer import with file - ok"),
    {ok, ?FI_OK} = 
        rct_proxy:send_proxy(node1, fi1, ?FiImportFile,
                     {Password, 
                      Uri ++ ?FiFile,
                  "test.xml"}),
    ok = check_result_on_node(os:getenv("SIM_OR_TARGET"), "/test.xml"),


    ct:pal("File transfer import with dir and file - ok"),
    {ok, ?FI_OK} = 
        rct_proxy:send_proxy(node1, fi1, ?FiImportFile,
                     {Password, 
                      Uri ++?FiFile,
                  "applicationlogs/test.xml"}),
    ok = check_result_on_node(os:getenv("SIM_OR_TARGET"), "/applicationlogs/test.xml"),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% 
%% @spec import_file_encrypt(Config) -> ok
%% @end
%%--------------------------------------------------------------------
import_file_encrypt(Config) ->
    SysPath = ?config(sys_path, Config),   
    Uri = ?config(uri_import, Config),
    Password = ?config(password, Config),
    Proto = ?config(ftp_protocol, Config),
    Host = ?config(host, Config),
    
    case Proto of
        sftp ->  {ok,_} = file:copy(?TestDir++?FiFile, SysPath++"/"++?FiFile);
        ftpes -> {ok, Binary} = file:read_file(?TestDir++?FiFile),
                 ok = rct_ftpes_client:write_file(SysPath++"/"++?FiFile, Binary)
    end,

    EncryptPassword = rct_rpc:call(rpc, comsaI, encrypt_password, [list_to_binary(Password)], 5000),

    ct:pal("File transfer import - ok"),
    {ok, ?FI_OK} = 
        rct_proxy:send_proxy(node1, fi1, ?FiImportFile2,
                     {EncryptPassword, 
                      Uri ++ ?FiFile,
                  "applicationlogs/"}),
    ok = check_result_on_node(os:getenv("SIM_OR_TARGET"), "/applicationlogs/"++?FiFile),

    %% ct:pal("File transfer import with faulty fullpath"),
    %% {ok, ?FI_FAILED_TO_IMPORT_OR_EXPORT_FILE} = 
    %%  rct_proxy:send_proxy(node1, fi1, ?FiImportFile2,
    %%               {EncryptPassword, 
    %%                "sftp://"++User++"@"++Sftp++?ProjDir++JenkinsNode++"/"++?FiFile,
    %%                "/applicationlogs/"}),

    ct:pal("File transfer import with faulty URI"),
    {ok, ?FI_INVALID_URI} = 
        rct_proxy:send_proxy(node1, fi1, ?FiImportFile2,
                     {EncryptPassword, 
                     "//proj/rcs-tmp/"++?FiFile,
                      ?FiFile}),

    ct:pal("File transfer import with password not possbile to decrypt"),
    {ok, ?FI_FAILED_TO_DECRYPT_PASSWORD} = 
        rct_proxy:send_proxy(node1, fi1, ?FiImportFile2,
                     {"wrong_password", 
                      Uri++?FiFile,
                  ?FiFile}),
    
    case Proto of 
        sftp ->
            ct:pal("File transfer import with wrong user"),
            {ok, ?FI_FAILED_TO_CONNECT_TO_SFTP_SERVER} = 
                rct_proxy:send_proxy(node1, fi1, ?FiImportFile2,
                    {EncryptPassword, 
                     "sftp://wrong_user@"++Host++SysPath++"/"++?FiFile,
                    ?FiFile});
        ftpes -> ok
    end,

    ct:pal("File transfer import with file - ok"),
    {ok, ?FI_OK} = 
        rct_proxy:send_proxy(node1, fi1, ?FiImportFile2,
                     {EncryptPassword, 
                      Uri++?FiFile,
                  "test.xml"}),
    ok = check_result_on_node(os:getenv("SIM_OR_TARGET"), "/test.xml"),


    ct:pal("File transfer import with dir and file - ok"),
    {ok, ?FI_OK} = 
        rct_proxy:send_proxy(node1, fi1, ?FiImportFile2,
                     {EncryptPassword, 
                      Uri++?FiFile,
                  "applicationlogs/test.xml"}),
    ok = check_result_on_node(os:getenv("SIM_OR_TARGET"), "/applicationlogs/test.xml"),
    ok.



%%--------------------------------------------------------------------
%% @doc 
%% 
%% @spec import_file3(Config) -> ok
%% @end
%%--------------------------------------------------------------------
import_file3(Config) ->
    SysPath = ?config(sys_path, Config),   
    Uri = ?config(uri_import, Config),
    Password = ?config(password, Config),
    Proto = ?config(ftp_protocol, Config),
    Host = ?config(host, Config),
    
    case Proto of
        sftp ->  {ok,_} = file:copy(?TestDir++?FiFile, SysPath++"/"++?FiFile);
        ftpes -> {ok, Binary} = file:read_file(?TestDir++?FiFile),
                 ok = rct_ftpes_client:write_file(SysPath++"/"++?FiFile, Binary)
    end,

    EncryptPassword = rct_rpc:call(rpc, comsaI, encrypt_password, [list_to_binary(Password)], 5000),

    ct:pal("File transfer import *3* - ok"),
    {ok, ?FI_OK} = 
        rct_proxy:send_proxy(node1, fi1, ?FiImportFile3,
                     {EncryptPassword, 
                      Uri ++ ?FiFile,
                  "applicationlogs/",
		  20000,
		  1}),
    ok = check_result_on_node(os:getenv("SIM_OR_TARGET"), "/applicationlogs/"++?FiFile),

   
    ct:pal("File transfer import with faulty URI...."),
    {ok, ?FI_INVALID_URI} = 
        rct_proxy:send_proxy(node1, fi1, ?FiImportFile3,
                     {EncryptPassword, 
                     "//proj/rcs-tmp/"++?FiFile,
                      ?FiFile,
		      20000,
		      1}),

    ct:pal("File transfer import with password not possbile to decrypt"),
    {ok, ?FI_FAILED_TO_DECRYPT_PASSWORD} = 
        rct_proxy:send_proxy(node1, fi1, ?FiImportFile3,
                     {"wrong_password", 
                      Uri++?FiFile,
                  ?FiFile,
		  20000,
		  1}),
    
    case Proto of 
        sftp ->
            ct:pal("File transfer import with wrong user"),
            {ok, ?FI_FAILED_TO_CONNECT_TO_SFTP_SERVER} = 
                rct_proxy:send_proxy(node1, fi1, ?FiImportFile3,
                    {EncryptPassword, 
                     "sftp://wrong_user@"++Host++SysPath++"/"++?FiFile,
                    ?FiFile,
		    20000,
		    1});
        ftpes -> ok
    end,

    ct:pal("File transfer import with file - ok"),
    {ok, ?FI_OK} = 
        rct_proxy:send_proxy(node1, fi1, ?FiImportFile3,
                     {EncryptPassword, 
                      Uri++?FiFile,
                  "test.xml",
		  20000,
		  1}),
    ok = check_result_on_node(os:getenv("SIM_OR_TARGET"), "/test.xml"),


    ct:pal("File transfer import with dir and file - ok"),
    {ok, ?FI_OK} = 
        rct_proxy:send_proxy(node1, fi1, ?FiImportFile3,
                     {EncryptPassword, 
                      Uri++?FiFile,
                  "applicationlogs/test.xml",
		  20000,
		  1}),
    ok = check_result_on_node(os:getenv("SIM_OR_TARGET"), "/applicationlogs/test.xml"),

    ct:pal("File transfer import exceeds the size limitation"),
    {ok, ?FI_FILE_SIZE_EXCEED_LIMITATION} = 
        rct_proxy:send_proxy(node1, fi1, ?FiImportFile3,
                     {EncryptPassword, 
                      Uri ++ ?FiFile,
                  "applicationlogs/",
		  1000,
		  1}),
    ok = check_result_on_node(os:getenv("SIM_OR_TARGET"), "/applicationlogs/test.xml"),
  ok.

%%--------------------------------------------------------------------
%% @doc 
%% 
%% @spec export_file(Config) -> ok
%% @end
%%--------------------------------------------------------------------
export_file(Config) ->
    User = ?config(username, Config),   
    Uri = ?config(uri_export, Config),
    Password = ?config(password, Config),
    Proto = ?config(ftp_protocol, Config),
    Host = ?config(host, Config),
    RcsDir = rct_rpc:call(rpc, sysEnv, rcs_dir, [], 5000),
    JenkinsNode = lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
 
    ct:pal("File transfer export - ok"),
    {ok, ?FI_OK} = 
	rct_proxy:send_proxy(node1, fi1, ?FiExportFile,
			     {Password, 
			      Uri,
			      RcsDir ++ "/fi/applicationlogs/"++?FiFile}),
    
    ok = check_result_on_server(Proto, 
                                case Proto of
                                    sftp -> "/backup/sftp/fi/log."++JenkinsNode;
                                   ftpes -> ?config(sys_path, Config)++ "/"++?FiFile
                               end, 
                               Config),

    ct:pal("File transfer export with faulty URI"),
    {ok, ?FI_INVALID_URI} = 
    	rct_proxy:send_proxy(node1, fi1, ?FiExportFile,
    			     {Password, 
			      "//proj/rcs-tmp/"++?FiFile,
    			      RcsDir ++ "/fi/applicationlogs/"++?FiFile}),

    ct:pal("File transfer export with wrong password"),
    {ok, ?FI_FAILED_TO_CONNECT_TO_SFTP_SERVER} = 
    	rct_proxy:send_proxy(node1, fi1, ?FiExportFile,
    			     {"wrong_password", 
    			     Uri,
			      RcsDir ++ "/fi/applicationlogs/"++?FiFile}),
    
    ct:pal("File transfer export with unknown dir at server - nok"),
    {ok, ?FI_FAILED_TO_IMPORT_OR_EXPORT_FILE} = 
        rct_proxy:send_proxy(node1, fi1, ?FiExportFile,
                     {Password, 
                      Uri ++ "/unknown/",
                  RcsDir ++ "/fi/applicationlogs/"++?FiFile}),
    
    case Proto of 
        sftp ->
            ct:pal("File transfer export with wrong user"),
            {ok, ?FI_FAILED_TO_CONNECT_TO_SFTP_SERVER} = 
            	rct_proxy:send_proxy(node1, fi1, ?FiExportFile,
            			     {Password, 
            			      "sftp://wrong_user@"++Host++"/backup/sftp/fi/log."++JenkinsNode,
        			      RcsDir ++ "/fi/applicationlogs/"++?FiFile}),
        
            ct:pal("File transfer export with unknown file - nok"),
            {ok, ?FI_FAILED_TO_IMPORT_OR_EXPORT_FILE} = 
            	rct_proxy:send_proxy(node1, fi1, ?FiExportFile,
            			     {Password, 
            			      Uri,
        			      "unknown_file.txt"}),
        
            ct:pal("File transfer export with no file in URI - ok"),
            {ok, ?FI_OK} = 
            	rct_proxy:send_proxy(node1, fi1, ?FiExportFile,
            			     {Password, 
            			      "sftp://"++User++"@"++Host++"/backup/sftp/fi/",
        			      RcsDir ++ "/fi/applicationlogs/"++?FiFile}),
         
            case check_result_on_sftp("/backup/sftp/fi/"++?FiFile) of
        	nok ->
        	    ct:pal("The result is not the expected, case env sim - try ones more since this is "
        		   "probably due to several tests runing in paralell~n"),
        	    case os:getenv("SIM_OR_TARGET") of
        		"sim" ->
        		    {ok, ?FI_OK} = 
        			rct_proxy:send_proxy(node1, fi1, ?FiExportFile,
        					     {Password, 
        					      "sftp://"++User++"@"++Host++"/backup/sftp/fi/",
        					      RcsDir ++ "/fi/applicationlogs/"++?FiFile}),
        		    ok = check_result_on_sftp("/backup/sftp/fi/"++?FiFile);
        		"target" ->
        		    ct:fail("Not possible to export file without file name to server");
        		"cloudish" ->
        		    ct:fail("Not possible to export file without file name to server")
        	    end;
        	ok ->
        	    ok
            end;
        ftpes -> ok
    end,
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% 
%% @spec export_file_encrypt(Config) -> ok
%% @end
%%--------------------------------------------------------------------
export_file_encrypt(Config) ->
    RcsDir = rct_rpc:call(rpc, sysEnv, rcs_dir, [], 5000),
    JenkinsNode = lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
    User = ?config(username, Config),   
    Uri = ?config(uri_export, Config),
    Password = ?config(password, Config),
    Proto = ?config(ftp_protocol, Config),
    Host = ?config(host, Config),
    EncryptPassword = rct_rpc:call(rpc, comsaI, encrypt_password, [list_to_binary(Password)], 5000),

    ct:pal("File transfer export - ok"),
    {ok, ?FI_OK} = 
	rct_proxy:send_proxy(node1, fi1, ?FiExportFile2,
			     {EncryptPassword, 
			      Uri,
			      RcsDir ++ "/fi/applicationlogs/"++?FiFile}),
    
    ok = check_result_on_server(Proto, 
                                case Proto of
                                    sftp -> "/backup/sftp/fi/log."++JenkinsNode;
                                   ftpes -> ?config(sys_path, Config)++ "/"++?FiFile
                               end, Config),

    ct:pal("File transfer export with faulty URI"),
    {ok, ?FI_INVALID_URI} = 
    	rct_proxy:send_proxy(node1, fi1, ?FiExportFile2,
    			     {EncryptPassword, 
			      "//proj/rcs-tmp/"++?FiFile,
    			      RcsDir ++ "/fi/applicationlogs/"++?FiFile}),

    ct:pal("File transfer export with password not possible to decrypt"),
    {ok, ?FI_FAILED_TO_DECRYPT_PASSWORD} = 
    	rct_proxy:send_proxy(node1, fi1, ?FiExportFile2,
    			     {"wrong_password", 
    			      Uri,
			      RcsDir ++ "/fi/applicationlogs/"++?FiFile}),

    ct:pal("File transfer export with unknown dir at server - nok"),
    {ok, ?FI_FAILED_TO_IMPORT_OR_EXPORT_FILE} = 
    	rct_proxy:send_proxy(node1, fi1, ?FiExportFile2,
    			     {EncryptPassword, 
    			      Uri ++ "unknown/",
			      RcsDir ++ "/fi/applicationlogs/"++?FiFile}),
    case Proto of 
        sftp -> 
            ct:pal("File transfer export with wrong user"),
            {ok, ?FI_FAILED_TO_CONNECT_TO_SFTP_SERVER} = 
                rct_proxy:send_proxy(node1, fi1, ?FiExportFile2,
                             {EncryptPassword, 
                              "sftp://wrong_user@"++Host++"/backup/sftp/fi/log."++JenkinsNode,
                          RcsDir ++ "/fi/applicationlogs/"++?FiFile}),
            
            ct:pal("File transfer export with unknown file - nok"),
            {ok, ?FI_FAILED_TO_IMPORT_OR_EXPORT_FILE} = 
                rct_proxy:send_proxy(node1, fi1, ?FiExportFile2,
                             {EncryptPassword, 
                              Uri,
                          "unknown_file.txt"}),
        
            ct:pal("File transfer export with no file in URI - ok"),
            {ok, ?FI_OK} = 
            	rct_proxy:send_proxy(node1, fi1, ?FiExportFile2,
            			     {EncryptPassword, 
            			      "sftp://"++User++"@"++Host++"/backup/sftp/fi/",
        			      RcsDir ++ "/fi/applicationlogs/"++?FiFile}),
         
            case check_result_on_sftp("/backup/sftp/fi/"++?FiFile) of
        	nok ->
        	    ct:pal("The result is not the expected, case env sim - try ones more since this is "
        		   "probably due to several tests runing in paralell~n"),
        	    case os:getenv("SIM_OR_TARGET") of
        		"sim" ->
        		    {ok, ?FI_OK} = 
        			rct_proxy:send_proxy(node1, fi1, ?FiExportFile2,
        					     {EncryptPassword, 
        					      "sftp://"++User++"@"++Host++"/backup/sftp/fi/",
        					      RcsDir ++ "/fi/applicationlogs/"++?FiFile}),
        		    ok = check_result_on_sftp("/backup/sftp/fi/"++?FiFile);
        		"target" ->
        		    ct:fail("Not possible to export file without file name to server");
        		"cloudish" ->
        		    ct:fail("Not possible to export file without file name to server")
        	    end;
        	ok ->
        	    ok
            end;
        ftpes ->
            ok
    end,
    ok.

test_writeRamFiles(_Config) ->
    % millisecond duration of a "simulated second"; set to
    % a value less than 1000 to obtain speedup.
    Sec = 500,
    
    SpaceLimit = (18*?TEST_FILE_SIZE) div 10,   % 1.8 * TEST_FILE_SIZE
    
    % force any junk files to be deleted quickly
    setFiRamdiskParameters(false, 0, 0, 1*Sec, 1*Sec), 
    timer:sleep(3*Sec),
    % at this point we trust that junk files are wiped
    
    % set parameters for actual test case
    setFiRamdiskParameters(true, 0, SpaceLimit, 20*Sec, 2*Sec),
    
    {ok, ?FI_OK} = rct_proxy:send_proxy(node1, fi1, ?FiOpenWrite, {?RAM_TEST_DIR, "f1", ?TEST_FILE_SIZE}),
    ct:pal("written file: ~s/f1", [?RAM_TEST_DIR]),
    
    timer:sleep(5*Sec),
    {ok, ?FI_OK} = rct_proxy:send_proxy(node1, fi1, ?FiOpenWrite, {?RAM_TEST_DIR, "f2", ?TEST_FILE_SIZE}),
    ct:pal("written file: ~s/f2", [?RAM_TEST_DIR]),
    ct:pal("SFTP directory /~s contains: ~p", [?RAM_TEST_DIR, rct_sftp_client:list_dir(?RAM_TEST_DIR)]),
    
    timer:sleep(5*Sec),
    {error, ?FI_SPACE_FULL} = rct_proxy:send_proxy(node1, fi1, ?FiOpenWrite, {?RAM_TEST_DIR, "f3", ?TEST_FILE_SIZE}),
    ct:pal("'file space full' condition verified", []),
    
    timer:sleep(16*Sec),
    ok = rct_rpc:call(rpc, sysFi, delete_older, [?RAM_TEST_DIR, 8], 5000),
    {ok, ?FI_OK} = rct_proxy:send_proxy(node1, fi1, ?FiOpenWrite, {?RAM_TEST_DIR, "f4", ?TEST_FILE_SIZE}),
    ct:pal("written file: ~s/f4", [?RAM_TEST_DIR]),
    verifyFileContent(rct_sftp_client:read_file("/" ++ ?RAM_TEST_DIR ++ "/f4")),
    ct:pal("verified content of file: ~s/f4", [?RAM_TEST_DIR]),
    
    timer:sleep(5*Sec),
    ct:pal("SFTP delete.. /~s/f4 returned: ~p", [?RAM_TEST_DIR, rct_sftp_client:delete_file("/" ++ ?RAM_TEST_DIR ++ "/f4")]),
    
    % restore default parameters
    setFiRamdiskParameters(true, 0, 0, 0, 0),
    rct_coli:disconnect(coli),
    
    ct:pal("test_writeRamFiles ends; no files expected to remain", []),
    ok.


%%% ----------------------------------------------------------
%%% @doc Simple tests of the Erlang interface for creating persistent
%%% files under the SFTP root directory. These tests use rct_rcp.
%%% Created directories are not removed; directory names are
%%% randomized and unlikely to clash with anything else.
%%% @end
%%% ----------------------------------------------------------
test_writeSsdFiles(_Config) ->
    RpcTimeout = 5000,
    
    DirName =
	lists:flatten(
	  io_lib:format("~8.16.0b", [rand:uniform((1 bsl 32)-1)])),

    % Register sftp directory 
    ok = rct_rpc:call(rpc, sysFi, register_sftp_dir, [DirName], RpcTimeout),
    ct:pal("Registered directory: ~s", [DirName]),
    
    {ok, DirAbsName} = 
	rct_rpc:call(rpc, sysFi, reg_absname, [DirName], RpcTimeout),
    ct:pal("directory absname is: ~s", [DirAbsName]),
    
    % Get directory info
    DirInfo = rct_rpc:call(rpc, sysFi, read_file_info, [DirName, none, []], RpcTimeout),
    ct:pal("get directory info, response: ~p", [DirInfo]),
    
    % Create a file
    FileName = "f1",
    FileContent = <<"qwertyuiop\n">>,
    ok = rct_rpc:call(rpc, sysFi, write_file, [DirName, FileName, FileContent], RpcTimeout),
    ct:pal("file created", []),

    % Get file info
    FileInfo = rct_rpc:call(rpc, sysFi, read_file_info, [DirName, FileName, []], RpcTimeout),
    ct:pal("get file info, response: ~p", [FileInfo]),
    
    % Verify file content
    {ok, FileContent} = rct_sftp_client:read_file(filename:join(DirName, FileName)),
    ct:pal("SFTP get was successful", []),
    
    % Delete the file
    ok = rct_rpc:call(rpc, sysFi, delete, [DirName, FileName], RpcTimeout),

    ct:pal("Removed file ~s", [FileName]),

    % Unegister sftp directory 
    ok = rct_rpc:call(rpc, gen_server, call, 
		      [sysFi, {unregister_sftp_dir, DirName}], RpcTimeout),
    ct:pal("Unregistered directory: ~s", [DirName]),
    ok.

check_result_on_node("target", File) ->
    ok = rct_rs232:login(console),
    RcsDir = rct_rpc:call(rpc, sysEnv, rcs_dir, [], 5000),
    {ok, [_Cmd | Answer]} = ct_telnet:cmd(console,"ls -l "++ RcsDir ++ "/fi" ++ File),
    case lists:nth(1, string:tokens(lists:nth(1, Answer), "/")) of
	"ls: cannot access " ->
	    %% The file does not exists
	    nok;
	_Else ->
	    ok
    end;
check_result_on_node("sim", File) ->
    RcsDir = rct_rpc:call(rpc, sysEnv, rcs_dir, [], 5000),
    %% Check if the file exists
    case lists:nth(1, string:tokens(os:cmd("ls -l " ++ RcsDir ++ "/fi" ++ File), "/")) of
	"ls: cannot access " ->
	    %% The file does not exists
	    nok;
	_Else ->
	    ok
    end;
check_result_on_node("cloudish", File) ->
    RcsDir = rct_rpc:call(rpc, sysEnv, rcs_dir, [], 5000),
    {ok,Answer} = ct_ssh:exec(ssh,"ls -l "++ RcsDir ++ "/fi" ++ File,5000),
    case lists:nth(1, string:tokens(Answer, "/")) of
	"ls: cannot access " ->
	    %% The file does not exists
	    nok;
	_Else ->
	    ok
    end.

check_result_on_sftp(FilePath) ->
    [{host, Sftp},{username, User},{password, Password}] = ct:get_config(sftp_server),
    {ok,ChPid,_ChRef} = 
	ssh_sftp:start_channel(Sftp, 
			       [{user, User},
				{password, Password},
				{silently_accept_hosts, true},
				{timeout, 60000}]),
    File = filename:basename(FilePath),
    Dir = filename:dirname(FilePath),
    
    {ok, FileNames} = ssh_sftp:list_dir(ChPid, Dir, 10000),
    case lists:member(File, FileNames) of
	true ->
	    %% The file does exist, delete it and return ok
	    ssh_sftp:delete(ChPid, FilePath, 10000),
	    ok;
	false ->
	    ct:pal("Not able to find the File: ~p. Files located on sftp are: ~p~n", 
		   [File, FileNames]),
	    nok
    end.

check_result_on_server(sftp, FilePath, Config) ->
    User = ?config(username, Config),   
    Password = ?config(password, Config),
    Host = ?config(host, Config),
    
    {ok,ChPid,_ChRef} = 
    ssh_sftp:start_channel(Host, 
                   [{user, User},
                {password, Password},
                {silently_accept_hosts, true},
                {timeout, 60000}]),
    File = filename:basename(FilePath),
    Dir = filename:dirname(FilePath),
    
    {ok, FileNames} = ssh_sftp:list_dir(ChPid, Dir, 10000),
    case lists:member(File, FileNames) of
    true ->
        %% The file does exist, delete it and return ok
        ssh_sftp:delete(ChPid, FilePath, 10000),
        ok;
    false ->
        ct:pal("Not able to find the File: ~p. Files located on sftp are: ~p~n", 
           [File, FileNames]),
        nok
    end;

check_result_on_server(ftpes, FilePath, _Config) ->
    ct:pal("~p",[FilePath] ),
    File = filename:basename(FilePath),
    Dir = filename:dirname(FilePath),
    {ok, RawDirData} = rct_ftpes_client:list_dir(Dir),
    FileNames = [filename:basename(string:strip(FileNames)) || FileNames <- string:tokens(RawDirData, "\r\n")],

    case lists:member(File, FileNames) of
    true ->
        %% The file does exist, delete it and return ok
        rct_ftpes_client:delete_file(FilePath),
        ok;
    false ->
        ct:pal("Not able to find the File: ~p. Files located on ftpes are: ~p~n", 
           [File, FileNames]),
        nok
    end.
 
%%--------------------------------------------------------------------
%% @doc Verify file content. This code must be aligned with the
%% test_fi module (the letter 'A' is hardcoded there).
%% @end
%%--------------------------------------------------------------------
-spec verifyFileContent({ok, binary()}) -> true.
	
verifyFileContent({ok, Binary}) ->
    String = binary_to_list(Binary),
    ?TEST_FILE_SIZE = length(String),
    true = lists:all(fun(X) -> X =:= $\A end, String).


%%--------------------------------------------------------------------
%% @doc Set parameters for the SFTP ramdisk file service.
%% FIXME: Once RCT has been re-released there is no need to
%% specify the prompt in the 'connect' operation. 
%% @end
%%--------------------------------------------------------------------
setFiRamdiskParameters(Connected, Persistence, SpaceLimit, StaleLimit, TimeoutMillis) ->
    if
	Connected ->
	    ok;
	true ->
	    % ok = rct_coli:connect(coli),
	    ok = rct_coli:connect(coli, "expert", "expert", "coli \\[.*]->", noprint),
	    rct_coli:send(coli, "/misc/authlevel disabled")
    end,
    rct_coli:send(coli, 
		  lists:flatten(
		    io_lib:format(
		      "/pes/ramfileparms ~w ~w ~w ~w",
		      [Persistence, SpaceLimit, StaleLimit, TimeoutMillis]))).

get_ftp_config(sftp) ->
    ct:get_config(sftp_server);
get_ftp_config(ftpes) ->
    [{host, ftpes_test_lib:get_ftpes_test_server_address(ipv4)},
     {username, ftpes_test_lib:get_ftpes_test_server_username()},
     {password, ftpes_test_lib:get_ftpes_test_server_password()}].

get_ftp_uri_import(sftp, User, Host) ->
    JenkinsNode = lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
     "sftp://"++User++"@"++Host++?ProjDir++JenkinsNode ++ "/";
get_ftp_uri_import(ftpes, User, Host) ->
    {ok, SysDir} = rct_ftpes_client:pwd(),
    "ftpes://"++User++"@"++Host ++ SysDir ++ "/".

get_ftp_uri_export(sftp, User, Host) ->
    JenkinsNode = lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
     "sftp://"++User++"@"++Host++"/backup/sftp/fi/log."++JenkinsNode;
get_ftp_uri_export(ftpes, User, Host) ->
    get_ftp_uri_import(ftpes, User, Host).


get_sys_path(sftp) ->
    JenkinsNode = lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
    ?ProjDir++JenkinsNode;
get_sys_path(ftpes) ->
    {ok, SysDir} = rct_ftpes_client:pwd(),
    SysDir.

ftpes_hook() ->
    {rct_ftpes_client, [{port, 21}, {ip, ftpes_test_lib:get_ftpes_test_server_address(ipv4)},
                        {user, ftpes_test_lib:get_ftpes_test_server_username()},
                        {password, ftpes_test_lib:get_ftpes_test_server_password()},
                        {start_session_per_tc, false}]}.


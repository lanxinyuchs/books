%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ftpes_client_suite.erl %
%%% Author: ekurnik
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------

-module(ftpes_client_suite).

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
%%% R8A/1    2016-11-18   ekurnik    Created
%%% R8A/2    2016-11-24   emarnek    Cleaned comments, added specs/docs
%%% R8A/8    2016-12-14   enekdav    Added code coverage
%%% R8A/12   2017-01-04   ekurnik    Added support for using designated FTPES dir
%%% R8A/13   2017-01-11   emarnek    Added NLST and rename tests
%%% R8A/14   2017-01-17   ekurnik    Switched to ftpes_test_lib in RCT
%%%--------------------------------------------------------------------

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         init_per_group/2,
         end_per_group/2,
         groups/0,
         all/0
       ]).

-export([
         login/1,
         login_without_security/1,
         quit/1,
         retrive_a_file/1,
         file_not_found/1,
         store_a_file/1,
         store_to_existing_file/1,
         successful_folder_creation/1,
         unsuccessful_folder_creation/1,
   	     successful_file_deletion/1,
         unsuccessful_file_deletion/1,
         successful_folder_deletion/1,
         unsuccessful_folder_deletion/1,
         successful_print_of_current_directory/1,
         execute_FTP_command_without_login/1,
         successful_listing_of_current_directory/1,
         successful_listing_of_specified_directory/1,
         successful_nlisting_of_current_directory/1,
         unsuccessful_listing_of_files/1,
         successful_directory_change/1,
         unsuccessful_directory_change/1,
         send_file_in_chunks/1,
         successful_rename_of_a_file/1,
         unsuccessful_rename_of_a_file/1
         ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

-include("$RCT_TOP/test/lib/lib/esrc/ftpes_test_lib.hrl").

-spec suite() -> [tuple()].

suite() ->
   [{timetrap, {minutes, 10}},
    {ct_hooks, [{rct_rpc, rpc},
                {rct_scp, scp1},
                {rct_ssh,{ssh,[manual_connect]}},
                {rct_htmllink,[]},
                {rct_netconf,{nc1, html}},
                {rct_logging, {all,
                                [{erlang,
                                  {["ERROR REPORT","CRASH REPORT"],
                                   ["exception exit: unexpected_exit",
                                    "\\*\\* unexpected_exit"]}
                                }]}},
        {rct_core,[]},
        {cover_hook,[{du1, username}]}
           ]}].

all() ->
  [
    {group, ipv4_group},
    {group, ipv6_group},
    {group, ftp_extension_ipv4}
  ].

groups() ->
  Ipv4_group = ipv4_group(),
  Ipv6_group = ipv6_group(),
  ActiveGroup = active_group(),
  PassiveGroup = passive_group(),
  MinReqGroup = min_req(),
  OptReqGroup = opt_req(),
  ExtensionGroup = ftp_extension_ipv4(),
  [
  {ipv4_group, [], Ipv4_group},
  {ipv6_group, [], Ipv6_group},
  {ftp_extension_ipv4, [], ExtensionGroup},
  {active_group, [], ActiveGroup},
  {passive_group, [], PassiveGroup},
  {min_req, [], MinReqGroup},
  {opt_req, [], OptReqGroup}
  ].

ipv4_group() ->
  [
    {group, active_group},
    {group, passive_group}
  ].

ipv6_group() ->
  [
    {group, active_group},
    {group, passive_group}
  ].

ftp_extension_ipv4() ->
  [
    {group, active_group},
    {group, passive_group}
  ].

passive_group() ->
    [
      {group, min_req},
      {group, opt_req},
      login_without_security
    ].
active_group()  ->
    [
      {group, min_req},
      {group, opt_req},
      login_without_security
    ].

%% @hidden --- US 6 min req (client)
min_req() ->
  [
    login,
    quit,
    retrive_a_file,
    file_not_found,
    store_a_file,
    store_to_existing_file,
    send_file_in_chunks
  ].

  opt_req() ->
  [
    execute_FTP_command_without_login,
    successful_listing_of_current_directory,
    successful_listing_of_specified_directory,
    successful_nlisting_of_current_directory,
    unsuccessful_listing_of_files,
    successful_directory_change,
    unsuccessful_directory_change,
    successful_folder_creation,
    unsuccessful_folder_creation,
   	successful_file_deletion,
    unsuccessful_file_deletion,
    successful_folder_deletion,
    unsuccessful_folder_deletion,
    successful_print_of_current_directory,
    successful_rename_of_a_file,
    unsuccessful_rename_of_a_file,
    send_file_in_chunks
  ].

%% @hidden
init_per_suite(Config) ->
    User = "mate",
    DataDir = ?config(data_dir, Config),
    ok = ftpes_test_lib:register_sftp_dir(ssh, ?FTPES_TEST_DIR, 1024, ssh_sftpd_file),
    Started = ftpes_test_lib:start_server(),
    
    UpdatedConfig = ftpes_test_lib:initialize_nc_tc(Config),
    ftpes_test_lib:enable_ftpes_tls([{firstNC, "ftpes_test_login"}, {firstTC, "ftpes_test_login_tc"} |Config]),
    timer:sleep(10000),
    [{data_port,25600}, {port, ?DEFAULT_SERVER_CONTROL_CONNECTION_PORT}, {user, User}, {started, Started},
     {certificate, [{certfile, DataDir ++ "user_expert.crt"}, {keyfile, DataDir ++ "user_expert.key"}]}|UpdatedConfig].

%% @hidden
end_per_suite(Config) ->
    ftpes_test_lib:disable_ftpes_tls(),
    case ?config(started, Config) of
        no -> ftpes_test_lib:stop_ftpes_server(rpc),
              ftpes_test_lib:stop_ftpes_sup(rpc);
        yes -> ok
    end,
    ftpes_test_lib:clean_nc_tc(Config).

%% @hidden
init_per_group(ipv4_group, Config) ->
    Host =  ftpes_test_lib:get_node_ip(rpc, ipv4),
    if (Host == einval) -> {skip, "This group expects ipv4 address"};
                   true -> [{ipfamily, inet}, {ftp_extension, false}, {host, Host}| Config]
    end;
    

init_per_group(ftp_extension_ipv4, Config) ->
	Host =  ftpes_test_lib:get_node_ip(rpc, ipv4),
    if (Host == einval) -> {skip, "This group expects ipv4 address"};
                   true -> [{ipfamily, inet}, {ftp_extension, true}, {host, Host}| Config]
    end;
    

init_per_group(ipv6_group, Config) ->
    Host =  ftpes_test_lib:get_node_ip(rpc, oam, ipv6),
    if (Host == einval) -> {skip, "This group expects ipv6 address"};
                   true -> [{ipfamily, inet6}, {host, Host} | Config]
    end;
    

%% @hidden
init_per_group(passive_group, Config) ->
  [{mode, passive} | Config];

%% @hidden
init_per_group(active_group, Config) ->
  [{mode, active} | Config];

%% @hidden
init_per_group(_GroupName, Config) ->
  Config.

%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(login_without_security, Config) ->
  Host = ?config(host, Config),
  Port = ?config(port, Config),
  IpFamily = ?config(ipfamily, Config),
  Mode = ?config(mode, Config),
  Extension =?config(ftp_extension, Config),
  
  {ok, Pid} = ftp:open(Host,
                      [{port, Port},
                      {mode, Mode},
                      {ipfamily, IpFamily},
                      {ftp_extension, Extension}]),
  [{pid, Pid} | Config];

%% @hidden
init_per_testcase(retrive_a_file, Config) ->
  NewConfig = init_test(retrive_a_file, Config),
  create_file_server(NewConfig),
  NewConfig;

%% @hidden
init_per_testcase(store_a_file, Config) ->
  NewConfig = init_test(store_a_file, Config),
  create_file_client(),
  NewConfig;

%% @hidden
init_per_testcase(store_to_existing_file, Config) ->
  NewConfig = init_test(store_to_existing_file, Config),
  create_file_client(),
  NewConfig;

%% @hidden
init_per_testcase(successful_file_deletion, Config) ->
  NewConfig = init_test(successful_file_deletion, Config),
  create_file_server(NewConfig),
  NewConfig;

%% @hidden
init_per_testcase(successful_folder_deletion, Config) ->
  NewConfig = init_test(successful_folder_deletion, Config),
  create_folder(NewConfig),
  NewConfig;

%% @hidden
init_per_testcase(successful_listing_of_specified_directory, Config) ->
  NewConfig = init_test(successful_listing_of_specified_directory, Config),
  create_folder(NewConfig),
  NewConfig;

%% @hidden
init_per_testcase(successful_directory_change, Config) ->
  NewConfig = init_test(successful_directory_change, Config),
  create_folder(NewConfig),
  NewConfig;

%% @hidden
init_per_testcase(successful_rename_of_a_file, Config) ->
  NewConfig = init_test(successful_rename_of_a_file, Config),
  create_file_server(NewConfig),
  NewConfig;

%% @hidden
init_per_testcase(TestCase, Config) ->
  init_test(TestCase, Config).

%% @hidden
end_per_testcase(retrive_a_file, Config) ->
  delete_file_server(Config);

%% @hidden
end_per_testcase(store_a_file, Config) ->
  delete_file_server(Config);

%% @hidden
end_per_testcase(store_to_existing_file, Config) ->
  delete_file_server(Config);

%% @hidden
end_per_testcase(successful_folder_creation, Config) ->
  delete_folder(Config);

%% @hidden
end_per_testcase(successful_listing_of_specified_directory, Config) ->
  delete_folder(Config);

%% @hidden
end_per_testcase(successful_directory_change, Config) ->
  Pid = ?config(pid, Config),
  ftp:cd(Pid, ".."),
  delete_folder(Config);

%% @hidden
end_per_testcase(unsuccessful_folder_creation, Config) ->
  delete_folder(Config);

%% @hidden
end_per_testcase(_TestCase, Config) ->
    Pid = ?config(pid, Config),
    ftp:close(Pid).

%% @hidden TC-US6-1, TC-US7.1-1*, TC-US8-1
%% Successful login sequence - according to use case 12.2
%% TC-US6-1, TC-US7.1-1* obsolite
login(Config) ->
  Pid = ?config(pid, Config),
  User = ?config(user, Config),
  ok = ftp:user(Pid, User, ""),
  ct:log("Successful login"),
  Config.

%% @hidden TC-US8-2, TC-US8-2  
%% Unsuccessful login sequence - login without security
login_without_security(Config) ->
  Pid = ?config(pid, Config),
  User = ?config(user, Config),
  {error,euser} = ftp:user(Pid, User, ""),
  ct:log("Unsuccessful login, reason euser"),
  Config.

%% @hidden TC-US6-2
%% Successful quit
quit(Config) ->
  Pid = ?config(pid, Config),
  ok = ftp:close(Pid),
  ct:log("Successful quit"),
  Config.

%% @hidden TC-US6-3
%% Successful retrieve of a file
retrive_a_file(Config) ->
  Pid = ?config(pid, Config),
  ok = ftp:recv(Pid, ?TEST_FILE_NAME),
  ct:log("Successful retrieve of a file"),
  Config.

%% @hidden TC-US6-3a
%% Unuccessful retrieve - file does not exist
file_not_found(Config) ->
  Pid = ?config(pid, Config),
  {error,epath} = ftp:recv(Pid, "non_existent_file"),
  ct:log("Unsuccessful retrieve of a file, epath"),
  Config.

%% @hidden TC-US6-4
%% Successful store
store_a_file(Config) ->
  Pid = ?config(pid, Config),
  ok = ftp:send(Pid, ?TEST_FILE_NAME),
  {ok, Content} = ftp:recv_bin(Pid, ?TEST_FILE_NAME),
  case (Content =:= <<?TEST_FILE_CONTENT>>) of
     true ->
            ct:log("Successful store"),
            ok;
     false -> ct:fail("Stor unsuccessful, wrong content in a file!")
  end,
  Config.

%% @hidden TC-US6-4a
%% Successful store to existing file
store_to_existing_file(Config) ->
  Pid = ?config(pid, Config),
  % stores into same filename two times expecting later stor content to persist
  ftp:send_bin(Pid, <<"Content to be overwritten">>, ?TEST_FILE_NAME),
  ok = ftp:send(Pid, ?TEST_FILE_NAME),
  {ok, Content} = ftp:recv_bin(Pid, ?TEST_FILE_NAME),
  case (Content =:= <<?TEST_FILE_CONTENT>>) of
     true -> ct:log("Successful store to existing file"),
             ok;
     false -> ct:fail("Testcase unsuccessful")
  end,
  Config.

%% @hidden send chunks
send_file_in_chunks(Config) ->
  Pid = ?config(pid, Config),
  N = 5, % number of chunks to send 
  ftp:send_chunk_start(Pid, ?TEST_FILE_NAME),
  help_send_chunks(Pid, N, list_to_binary(?TEST_FILE_CONTENT)),
  ftp:send_chunk_end(Pid),
  ExpectedContent = list_to_binary(lists:flatten(lists:duplicate(N, ?TEST_FILE_CONTENT))),
  {ok, ServerContent} = ftp:recv_bin(Pid, ?TEST_FILE_NAME),
  case (ExpectedContent =:= ServerContent) of
    true -> ok;
    false -> ct:fail("Content mismatch!")
  end,
  Config.

%% @hidden TC-US7.1-5
%% Successful file deletion
successful_file_deletion(Config) ->
   Pid = ?config(pid, Config),
   ok = ftp:delete(Pid, ?TEST_FILE_NAME),
   {ok, Listing} = ftp:ls(Pid),
   case string:str(Listing, ?TEST_FILE_NAME) of % if file is deleted it should not be listed
     0 -> ct:log("Successful delete file"),
          ok;
     _ -> ct:fail("File not deleted!")
   end,
   Config.

%% @hidden TC-US7.1-5a
%% Unsuccessful file deletion
 unsuccessful_file_deletion(Config) ->
   Pid = ?config(pid, Config),
   {error, _} = ftp:delete(Pid,  "non_existent_file_name" ),
   ct:log("File not deleted"),
   Config.

%% @hidden TC-US7.1-4
%% Successful folder creation
successful_folder_creation(Config) ->
   Pid = ?config(pid, Config),
   ok = ftp:mkdir(Pid, ?TEST_DIR_NAME),
   {ok, Listing} = ftp:ls(Pid),
   case string:str(Listing, ?TEST_DIR_NAME) of % if folder is created it should be listed
     0 -> ct:fail("Folder not created!");
     _ -> ok, ct:log("Successful create folder")
   end,
   Config.

%% @hidden TC-US7.1-4a
%% Unsuccessful folder creation
unsuccessful_folder_creation(Config) ->
   Pid = ?config(pid, Config),
   ftp:mkdir(Pid, ?TEST_DIR_NAME), % not in init_pet_testcase for test readability
   {error, _} = ftp:mkdir(Pid, ?TEST_DIR_NAME), % already created, hence unsuccessfull
   ct:log("Unsuccessful create folder"),
   Config.

%% @hidden TC-US7.1-6
%% Successful folder deletion
successful_folder_deletion(Config) ->
   Pid = ?config(pid, Config),
   ok = ftp:rmdir(Pid, ?TEST_DIR_NAME),
   {ok, Listing} = ftp:ls(Pid),
   case string:str(Listing, ?TEST_DIR_NAME) of % if folder is deleted it should not be listed
     0 -> ct:log("Successful delete folder"), ok;
     _ -> ct:fail("Folder not deleted!")
   end,
   Config.

%% @hidden TC-US7.1-6a
%% Unsuccessful folder deletion
unsuccessful_folder_deletion(Config) ->
  Pid = ?config(pid, Config),
  {error, _} = ftp:rmdir(Pid, "non_existent_test_dir"),
  ct:log("Unsuccessful folder deletion"),
  Config.

%% @hidden TC-US7.1-7
%% Successful print of current directory
successful_print_of_current_directory(Config) ->
  Pid = ?config(pid, Config),
  {ok, CurrDir} = ftp:pwd(Pid),
  ct:log("Successful pwd, returned: ~p", [CurrDir]),
  Config.

%% @hidden TC-7.1-1a*, TC-US8-3
%% Execute FTP command witout login
execute_FTP_command_without_login(Config) ->
  Pid = ?config(pid, Config),
  {error, LoginError} = ftp:pwd(Pid),
  ct:log("Unsuccessful FTP command, reason : ~p", [LoginError]),
  Config.

%% @hidden TC-7.1-2
%% Successful listing of current directory
successful_listing_of_current_directory(Config) ->
  Pid = ?config(pid, Config),
  {ok, ListData} = ftp:ls(Pid),
  ct:log("Successful listing, returned: ~p", [ListData]),
  Config.

%% @hidden TC-7.1-2a
%% Successful listing of specified directory
successful_listing_of_specified_directory(Config) ->
  Pid = ?config(pid, Config),
  {ok, ListData} = ftp:ls(Pid, ?TEST_DIR_NAME),
  ct:log("Successful listing of specified dir, returned: ~p", [ListData]),
  Config.

%% @hidden TC-7.1-8
%% Successful nlisting of current directory
successful_nlisting_of_current_directory(Config) ->
  Pid = ?config(pid, Config),
  {ok, ListData} = ftp:nlist(Pid),
  ct:log("Successful nlisting, returned: ~p", [ListData]),
  Config.

%% @hidden TC-7.1-2b
%% Unsuccessful listing of files
unsuccessful_listing_of_files(Config) ->
  Pid = ?config(pid, Config),
  {ok, List} = ftp:ls(Pid, "/non_existent_folder"), %% ebabmat: perhaps it should be {error, _Reason}?
  ct:log("Unsuccessful listing files, returned: ~p", [List]),
  Config.

%% @hidden TC-7.1-3
%% Successful directory change
% pwd before cd and pwd after cd and then compare the two after adding the expected dir on the first pwd result
successful_directory_change(Config) ->
  Pid = ?config(pid, Config),
  {ok, DirBefore} = ftp:pwd(Pid),
  ok = ftp:cd(Pid, ?TEST_DIR_NAME),
  {ok, DirAfter} = ftp:pwd(Pid),
  ct:log(DirAfter),
  ExpectedDir = DirBefore ++ "/" ++ ?TEST_DIR_NAME,
  case (ExpectedDir =:= DirAfter) of
     true -> ct:log("Successful dir change"), ok;
     false -> ct:fail("Dir change unsuccessfull, wrong resulting dir!")
  end,
  Config.

%% @hidden TC-7.1-3a
%% Unsuccessful directory change
unsuccessful_directory_change(Config) ->
  Pid = ?config(pid, Config),
  {error, DirectoryError} = ftp:cd(Pid, "/non_exiscertfiletent_folder"),
  ct:log("Unsuccessful dir change, reason: ~p", [DirectoryError]),
  Config.

%% @hidden TC-US7.1-9
%% Successful rename_of_a_file
successful_rename_of_a_file(Config) ->
   Pid = ?config(pid, Config),
   ok = ftp:rename(Pid, ?TEST_FILE_NAME, ?TEST_FILE_NAME ++ "2"),
   {ok, Listing} = ftp:ls(Pid),
   ct:log("Listing: ~p",[Listing]),
   Renamed = string:str(Listing, ?TEST_FILE_NAME ++ "2"),
   case Renamed of % if file is renamed it should be founded
        0 -> ct:fail("File not renamed!");
        _ -> ct:log("Successful rename file"),
             ok
   end,
   ok = ftp:delete(Pid, ?TEST_FILE_NAME ++ "2"),
   Config.

%% @hidden TC-US7.1-9a
%% Successful rename_of_a_file
unsuccessful_rename_of_a_file(Config) ->
   Pid = ?config(pid, Config),
   Reason = ftp:rename(Pid, ?TEST_FILE_NAME, ?TEST_FILE_NAME ++ "2"),
   ct:log("~p",[Reason]),
   Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% init_test/2
%% ====================================================================
%% @doc Client opens and logins to FTPES server

-spec init_test(TestCase :: atom(), 
                Config :: list()) -> list().
%% ====================================================================
init_test(TestCase, Config) ->
  Host = ?config(host, Config),
  Port = ?config(port, Config),
  IpFamily = ?config(ipfamily, Config),
  Mode = ?config(mode, Config),
  Extension =?config(ftp_extension, Config),
  Certificate = ?config(certificate, Config),
  {ok, Pid} = ftp:open(Host,
                      [{port, Port},
                      {mode, Mode},
                      {ipfamily, IpFamily},
                      {ftp_extension, Extension},
                      {tls, Certificate}
                      ]),
  NConfig = [{pid, Pid} | Config],
  %login necessary before each test case except for these two:
  case TestCase of
    execute_FTP_command_without_login ->
      NConfig;
    login -> 
      NConfig;
    _ -> 
      login(NConfig),
      ftp:cd(Pid, ?FTPES_TEST_DIR),
      NConfig
  end.
  

%% create_folder/1
%% ====================================================================
%% @doc Creating folder with ftp:mkdir

-spec create_folder(Config :: list()) -> ok | {error, _Reason}.
%% ====================================================================
create_folder(Config) ->
	Pid = ?config(pid, Config),
    ftp:mkdir(Pid, ?TEST_DIR_NAME),
    ct:log("Create folder for data test").

%% delete_folder/1
%% ====================================================================
%% @doc Deleting folder with ftp:rmdir and closing FTP connection

-spec delete_folder(Config :: list()) -> ok.
%% ====================================================================
delete_folder(Config) ->
    Pid = ?config(pid, Config),
    io:format("Pid: ~p",[Pid]),
    ftp:rmdir(Pid, ?TEST_DIR_NAME),
    ct:log("Delete folder after data test"),
    ftp:close(Pid).

%% create_file_server/1
%% ====================================================================
%% @doc

-spec create_file_server(Config :: list()) -> ok | {error, _Reason}.
%% ====================================================================
create_file_server(Config) ->
	Pid = ?config(pid, Config),
	create_file_client(),
	ftp:send(Pid, ?TEST_FILE_NAME),
    ct:log("Create file for data test").

%% delete_file_server/1
%% ====================================================================
%% @doc Deleting file with ftp:delete and closing FTP connection

-spec delete_file_server(Config :: list()) -> ok.
%% ====================================================================
delete_file_server(Config) ->
	Pid = ?config(pid, Config),
	ftp:delete(Pid, ?TEST_FILE_NAME),
    ct:log("Delete file after data test"),
    ftp:close(Pid).

%% create_file_client/0
%% ====================================================================
%% @doc If file exists (created by previous test) it gets overwritten, 
%% if it doesn't exist it gets created, therefore no need for delete_file_client
%% Consequence: in results folder for tests the test file remains after the
%% test suite has finished running... important?

-spec create_file_client() -> ok | {error, _Reason}.
%% ====================================================================
create_file_client() ->
	file:write_file(?TEST_FILE_NAME, ?TEST_FILE_CONTENT).

%% help_send_chunks/3
%% ====================================================================
%% @doc Transfering N chunks to the remote server

-spec help_send_chunks(Pid :: integer(),
                       N :: integer(),
                       BinChunk :: binary()) -> ok | {error, _Reason}.
%% ====================================================================
help_send_chunks(_, 0, _) -> ok;
help_send_chunks(Pid, N, BinChunk) ->
  ftp:send_chunk(Pid, BinChunk),
  help_send_chunks(Pid, N - 1, BinChunk).

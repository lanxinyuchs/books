%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	log_ram_test_lib.erl %
%%% @author uabesvi
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R5A/R6A/R8A/R11A/4
%%%
%%% @doc == Test Library for RAM Log.==
%%% This library contains helper functions for RAM Log
%%% test suites.
%%% <br/><br/>
%%% @end

-module(log_ram_test_lib).

%%% ----------------------------------------------------------
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
%%% R5V/1      2016-03-15 edamkon     Created
%%% ----------------------------------------------------------
%%%

-include_lib("common_test/include/ct.hrl").
-include("log_ram_test.hrl").

-export([is_string/1]).
-export([join_to_str/1]).
-export([stringify/1]).
-export([all_ok/1]).
-export([target_rpc/2]).
-export([target_rpc/3]).
-export([target_rpc/4]).
-export([logram_rpc/1]).
-export([logram_rpc/2]).
-export([logram_rpc/3]).
-export([create_log/0]).
-export([create_log/1]).
-export([create_log/2]).
-export([get_default_tag/0]).
-export([write_to_file/0]).
-export([write_to_file/1]).
-export([write_log/1]).
-export([write_log/2]).
-export([write_log/3]).
-export([write_log/4]).
-export([write_log_n/2]).
-export([write_log_multi/1]).
-export([write_log_multi/2]).
-export([write_log_multi/3]).
-export([set_severity/1]).
-export([set_severity/2]).
-export([set_options/1]).
-export([set_options/2]).
-export([delete_logs/0]).
-export([delete_logs/1]).
-export([get_rel_log_dir/0]).
-export([get_rel_log_dir/1]).
-export([get_log_dir/0]).
-export([get_log_dir/1]).
-export([read_file/1]).
-export([check_content/2]).
-export([check_content/3]).
-export([is_all_true/1]).
-export([get_priv_dir/1]).
-export([cleanup/1]).
-export([cleanup/2]).
-export([delete_logs_from_ram/1]).
-export([delete_extracted_esi/1]).
-export([print_ram_log_files/1]).
-export([print_ram_log_files/2]).
-export([apply_template/2]).
-export([replace_multi/2]).
-export([list_replace/3]).
-export([read_target_file/1]).
-export([get_target_log_paths/0]).
-export([get_target_log_paths/1]).
-export([get_target_log_contents/0]).
-export([get_target_log_contents/1]).
-export([get_target_log_contents/2]).
-export([check_log/1]).
-export([check_log/2]).
-export([check_log/3]).
-export([should_fail/2]).
-export([should_fail/3]).
-export([should_fail_multi/2]).
-export([should_fail_multi/3]).
-export([connect/0]).
-export([disconnect/0]).
-export([send/2]).
-export([connect_send/2]).
-export([coli_exec/2]).
-export([check_esi/3]).
-export([check_esi/4]).
-export([check_esi_content/3]).
-export([extract_esi/2]).
-export([export_esi/1]).
-export([check_esi_log_transfered/2]).
-export([get_sftp_info/0]).
-export([wait_for_progress/2]).
-export([extract_element/2]).
-export([log_contains_data/2]).
-export([log_contains_data/3]).
-export([format_log_content/1]).
-export([logslist_to_map/1]).
-export([logslist_to_map/2]).
-export([get_log_formatted/0]).
-export([get_log_formatted/1]).
-export([get_esi_content/1]).
-export([restart_node/1]).
-export([wait_until_node_up/1]).
-export([wait_until_node_up/2]).
-export([ping_node_until_up/1]).
-export([ping_node_until_up/2]).
-export([check_netconf/0]).
-export([get_timestamp/0]).
-export([get_duration_since/1]).
-export([reduce_duration/2]).


%%========================================================================
%% is_string([term()]) -> bool()
%%
%% Checks whether a given term is string (a list of integers).
%%========================================================================
is_string([X | _] = Term)
  when is_list(Term), is_integer(X) ->
  true;

is_string(_) ->
  false.

%%========================================================================
%% join_to_str([term()]) -> string()
%%
%% Join terms from a list to a single string, where each term
%% (converted to a string) is separated by space.
%%========================================================================
join_to_str(Terms) ->
  Strings = lists:map(fun stringify/1, Terms),
  string:join(Strings, " ").


%%========================================================================
%% stringify([term()]) -> string()
%%
%% Convert the given term to string.
%%========================================================================
stringify(Term) ->
  case is_string(Term) of
    true ->
      Term;
    false ->
      Str = io_lib:format("~p", [Term]),
      lists:flatten(Str)
  end.

%%========================================================================
%% all_ok([X]) -> ok | [X]
%%
%% Checks whether all the items in the provided list are ok. If not,
%% return that same list.
%%========================================================================
all_ok(Xs) ->
    Res = lists:all(fun (X) -> ok == X end, Xs),
    case Res of
        true -> ok;
           _ -> Xs
    end.


%%--------------------------------------------------------------------
%% target_rpc(M, F, A, Silent) -> ok.
%%
%% Silent - bool() - if the call via RPC will be logged to CT HTML
%%
%% Makes an RPC call to the given module running
%% on the target (node or sim).
%%--------------------------------------------------------------------
target_rpc(M, F) ->
  target_rpc(M, F, []).

target_rpc(M, F, A) ->
  target_rpc(M, F, A, false).

target_rpc(M, F, A, Silent) ->
  Print = case Silent of
    true -> noprint;
       _ -> print
  end,

  rct_rpc:call(rpc_1, M, F, A, ?RPC_TIMEOUT, Print).

%%--------------------------------------------------------------------
%% logram_rpc(F, A, Silent) -> ok.
%%
%% Makes an RPC call to the logRamI module running on the node.
%%--------------------------------------------------------------------
logram_rpc(F) ->
  target_rpc(?LOG_RAM, F).

logram_rpc(F, A) ->
  target_rpc(?LOG_RAM, F, A).

logram_rpc(F, A, Silent) ->
  target_rpc(?LOG_RAM, F, A, Silent).


%%--------------------------------------------------------------------
%% create_log(LogName, Options) -> ok.
%%
%% Creates a new RAM Log.
%%--------------------------------------------------------------------
create_log() ->
  create_log([]).

create_log(Options) ->
  create_log(?LOG_NAME, Options).

create_log(LogName, Options) ->
  logram_rpc(create_log, [LogName, Options]).

%%--------------------------------------------------------------------
%% get_default_tag() -> {Module, LineNumber, Pid}.
%%
%% Gets the default tag for use in blacklist. Used for backwards
%% compatibility with TCs that didn't implement proper tag handling
%% while writing to RAM Log.
%%--------------------------------------------------------------------
get_default_tag() ->
  {?MODULE, ?LINE, self()}.

%%--------------------------------------------------------------------
%% write_to_file(LogName) -> ok.
%%
%% Write log contents from RAM to a file.
%%--------------------------------------------------------------------
write_to_file() ->
  Errors = logram_rpc(write_to_file),

  case length(Errors) of
    0 -> ok;
    _ -> {error, Errors}
  end.

write_to_file(LogName) ->
  logram_rpc(write_to_file, [LogName]).

%%--------------------------------------------------------------------
%% write_log(Data) -> ok.
%%
%% Writes an entry into the log.
%%--------------------------------------------------------------------
write_log(Data) ->
  write_log(?LOG_NAME, Data).

write_log(LogName, Data) when is_list(LogName) ->
  Tag = get_default_tag(),
  write_log(LogName, Data, Tag);
  
write_log(Data, Tag) when is_tuple(Tag) ->
  write_log(?LOG_NAME, Data, Tag).

write_log(LogName, Data, Tag) ->
  write_log(LogName, Data, Tag, false).

write_log(LogName, Data, Tag, Silent) ->
  logram_rpc(write_log, [LogName, Tag, Data], Silent).


%%--------------------------------------------------------------------
%% write_log_n(Data, N) -> ok.
%%
%% Writes an entry N times into the log.
%%--------------------------------------------------------------------
write_log_n(Data, N) ->
  ct:pal("=== Started writing data to RAM Log ===~n"
         "Data : ~p~n"
         "No of times: ~p", 
         [Data, N]),

  Start = get_timestamp(),

  write_log_n(?LOG_NAME, Data, N, true),

  Duration = get_duration_since(Start),

  ct:pal("=== Finished writing data to RAM Log in ~pms ===", [Duration]).

%%write_log_n(Data, N, Silent) ->
%%  write_log_n(?LOG_NAME, Data, N, Silent).

write_log_n(LogName, Data, N, Silent) ->
  Tag = get_default_tag(),
  write_log_n(LogName, Data, N, Tag, Silent).

write_log_n(_LogName, _Data, 0, _Tag, _Silent) ->
  ok;

write_log_n(LogName, Data, N, Tag, Silent) ->
  Ret = write_log(LogName, Data, Tag, Silent),
  case Ret of
    ok ->
      write_log_n(LogName, Data, N - 1, Tag, Silent);
    _ ->
      Ret
  end.


%%--------------------------------------------------------------------
%% write_log_multi(Data) -> ok.
%%
%% Enables writing multiple DataElements in one function call
%% (multiple calls to write_log).
%%--------------------------------------------------------------------
write_log_multi(Data) ->
  write_log_multi(?LOG_NAME, Data).

write_log_multi(LogName, Data) ->
  Tag = get_default_tag(),
  write_log_multi(LogName, Data, Tag).

write_log_multi(LogName, Data, Tag) ->
  WriteLog = fun(DataElement) -> 
    write_log(LogName, DataElement, Tag)
  end,

  Results = lists:map(WriteLog, Data),
  all_ok(Results).


%%--------------------------------------------------------------------
%% set_severity(Severity) -> ok | error.
%%
%% Severity = int(),
%%
%% Sets RAM Log's severity level.
%%--------------------------------------------------------------------
set_severity(Severity) ->
  set_severity(?LOG_NAME, Severity).

set_severity(LogName, Severity) ->
  logram_rpc(set_severity, [LogName, Severity]).

%%--------------------------------------------------------------------
%% set_options() -> ok | error.
%%
%% Options : [Option]
%% Option : {size, Size} | {maxNoFiles, MaxNoFiles} |
%%          {header, Header} | {zip, Zip} | {seqNo, SeqNo}
%%
%% Sets RAM Log's options.
%%--------------------------------------------------------------------
set_options(Options) ->
  set_options(?LOG_NAME, Options).

set_options(LogName, Options) ->
  logram_rpc(set_options, [LogName, Options]).

%%--------------------------------------------------------------------
%% delete_logs(LogNames) -> string().
%%
%% LogNames = string() | [string()]
%%
%% Deletes all the files for the given RAM Log.
%%--------------------------------------------------------------------
delete_logs() ->
  delete_logs(?LOG_NAME).

delete_logs([Log | _] = Logs) when is_list(Logs), is_list(Log) ->
  Res = lists:map(fun delete_logs/1, Logs),
  all_ok(Res);

delete_logs(LogName) ->
  LogDir = get_log_dir(LogName),

  %% Create RAM Log directory if it doesn't yet exist.
  ok = target_rpc(filelib, ensure_dir, [LogDir ++ "/"]),

  {ok, Files} = target_rpc(file, list_dir, [LogDir]),
  IsDirEmpty  = length(Files) == 0,

  case (IsDirEmpty) of
    true ->
      ct:pal("RAM Log directory empty, no logs to delete..."),
      ok;
    false ->
      Delete = fun(File) -> 
        Path = filename:join(LogDir, File),
        target_rpc(file, delete, [Path])
      end,
      
      Res = lists:map(Delete, Files),
      all_ok(Res)
  end.


%%--------------------------------------------------------------------
%% get_log_dir(LogName) -> string().
%%
%% Gets the directory where the RAM Log is saved on disk.
%%--------------------------------------------------------------------
get_rel_log_dir() ->
    get_rel_log_dir(?LOG_NAME).

get_rel_log_dir(LogName) ->
    LogDir = get_log_dir(LogName),
    RcsRoot = target_rpc(sysEnv, rcs_root),
    LogDir -- RcsRoot.

%%--------------------------------------------------------------------
%% get_log_dir(LogName) -> string().
%%
%% Gets the directory where the RAM Log is saved on disk.
%%--------------------------------------------------------------------
get_log_dir() ->
    get_log_dir(?LOG_NAME).

get_log_dir(LogName) ->
    %% FIXME: Check if local log or not.
    Dir = get_rcs_or_vnf_dir(),
    filename:join([Dir, "log", LogName]).

get_rcs_or_vnf_dir() ->
    case target_rpc(sysEnv, vnf_dir) of
	VNFDir when is_list(VNFDir) ->
	    VNFDir;
	_Error ->    
	    target_rpc(sysEnv, rcs_dir)
    end.
	    

%%--------------------------------------------------------------------
%% read_file(AbsolutePath) -> Contents.
%%
%% AbsolutePath = string(),
%% Contents     = string(),
%%
%% Gets a string content of a single textual file by the
%% provided absolute path.
%%--------------------------------------------------------------------
read_file(FilePath) ->
  {ok, Bin} = file:read_file(FilePath),
  unicode:characters_to_list(Bin).


%%--------------------------------------------------------------------
%% check_content(Content, Expected, Unexpected) -> bool().
%%
%% Content    = string(),
%% Expected   = [string()],
%% Unexpected = [string()],
%%
%% Checks if the expected strings occured or not
%% in the given string.
%%--------------------------------------------------------------------
check_content(Content, Expected) ->
  check_content(Content, Expected, []).

check_content(Content, Expected, Unexpected) ->
  Exp   = [string:str(Content, Exp)  =/= 0 || Exp   <- Expected],
  Nexp  = [string:str(Content, Unexp) == 0 || Unexp <- Unexpected],
  is_all_true(Exp ++ Nexp).

%%--------------------------------------------------------------------
%% is_all_true([bool()]) -> bool().
%%
%% Check is the given list contains only true values.
%%--------------------------------------------------------------------
is_all_true(Xs) ->
  lists:all(fun(X) -> X == true end, Xs).


%%-------------------------------------------------------------------
%% Gets the CT Log's private folder.
%%-------------------------------------------------------------------
get_priv_dir(Config) ->
  ?config(priv_dir, Config).


%%-------------------------------------------------------------------
%% Remove CT's copy of ESI and target's logs.
%%-------------------------------------------------------------------
cleanup(Config) ->
  cleanup(Config, [?LOG_NAME]).

cleanup(Config, LogNames) ->
  delete_logs_from_ram(LogNames),
  delete_logs(LogNames),
  delete_extracted_esi(Config).


%%-------------------------------------------------------------------
%% Delete RAM Logs from system RAM.
%%----2---------------------------------------------------------------
delete_logs_from_ram(LogNames) ->
  DeleteLog = fun(LogName) -> 
    logram_rpc(delete_log, [LogName])
  end,
  
  lists:map(DeleteLog, LogNames).


%%-------------------------------------------------------------------
%% Remove CT's copy of ESI Log archives and extracted folders.
%%-------------------------------------------------------------------
delete_extracted_esi(Config) ->
  PrivDir  = get_priv_dir(Config),
  EsiFiles = filename:join(PrivDir, "*esi*"),
  RcsDir   = filename:join(PrivDir, "rcs"),
  os:cmd("rm -rf " ++ EsiFiles ++ " " ++ RcsDir).


%%-------------------------------------------------------------------
%% Adds RAM Log files to the CT log.
%%-------------------------------------------------------------------
print_ram_log_files(Config) ->
  print_ram_log_files(Config, ?LOG_NAME).

print_ram_log_files(Config, [Log | _] = Logs)
  when is_list(Log), is_list(Logs) ->
    Print = fun(LogName) ->
      print_ram_log_files(Config, LogName)
    end,

    lists:foreach(Print, Logs);

print_ram_log_files(Config, RamLogName) ->
  GetLogData = fun(LogPath) ->
    FileName = filename:basename(LogPath),
    Content  = read_target_file(LogPath),
    {FileName, Content}
  end,

  PrivDir        = get_priv_dir(Config),
  CopiedDirName  = filename:join([?COPIED_DIR_NAME, RamLogName]),
  CopiedDir      = filename:join([PrivDir, CopiedDirName]),
  
  ok = filelib:ensure_dir(CopiedDir),
  os:cmd("mkdir " ++ CopiedDir),

  LogPaths       = get_target_log_paths(RamLogName),
  LogData        = lists:map(GetLogData, LogPaths),

  WriteToFile = fun({FileName, Content}) ->
    Path = filename:join(CopiedDir, FileName),
    Bin  = list_to_binary(Content),
    file:write_file(Path, Bin),
    FileName
  end,

  WrittenFileNames = lists:map(WriteToFile, LogData),

  RelPrivPath    = lists:last(filename:split(PrivDir)),
  RelCopiedPath  = filename:join([RelPrivPath, CopiedDirName]),
  AbsCopiedPath  = filename:join([PrivDir,     CopiedDirName]),

  FileToLink = fun(FileName) ->
    RelFilePath = filename:join(RelCopiedPath, FileName),
    AbsFilePath = filename:join(AbsCopiedPath, FileName),
    {ok, Info}  = file:read_file_info(AbsFilePath),
    FileSize    = Info#file_info.size,

    Entry = [{name, FileName},
             {path, RelFilePath},
             {size, FileSize}],

    apply_template(?LOG_LINK_TPL, Entry)
  end,

  Links = lists:map(FileToLink, WrittenFileNames),

  ct:pal("=== RAM Log files for ~p ===~n~p", [RamLogName, Links]),
  ok.


%%-------------------------------------------------------------------
%% apply_template(Template, Data) -> string().
%%
%% Template = string()
%% Data     = [Entry]
%% Entry    = {Key, Value}
%% Key      = atom()
%% Value    = string()
%%
%% Replaces the placeholders inside of a template with actual
%% values.
%%-------------------------------------------------------------------
apply_template(Result, []) ->
  Result;

apply_template(Template, Data) ->
  TplParts = string:tokens(Template, "{}"),
  Replaced = replace_multi(TplParts, Data),
  string:join(Replaced, "").


%%-------------------------------------------------------------------
%% Replace multiple items in a list.
%%-------------------------------------------------------------------
replace_multi(Res, []) ->
  Res;

replace_multi(List, [{Match, Replace} | T]) ->
  Res = list_replace(List, Match, Replace),
  replace_multi(Res, T).


%%-------------------------------------------------------------------
%% Replace occurences of "Match" in the list with "Replace".
%%-------------------------------------------------------------------
list_replace(List, Match, Replace) ->
  list_replace(List, Match, Replace, []).

list_replace([], _, _, Res) ->
  lists:reverse(Res);

list_replace([Item | T], RawMatch, RawReplace, Acc) ->
  Match   = stringify(RawMatch),
  Replace = stringify(RawReplace),

  Res = case Item == Match of
          true  -> Replace;
          false -> Item
        end,

  list_replace(T, Match, Replace, [Res | Acc]).


%%========================================================================
%% remote_read_file(RemotePath) -> string()
%%
%% Reads contents of a file on target.
%%========================================================================
read_target_file(RemotePath) ->
  read_target_file(RemotePath, false).

read_target_file(RemotePath, AsBinary) ->
  {ok, BitString} = target_rpc(file, read_file, [RemotePath], true),

  case AsBinary of
    true -> BitString;
       _ -> binary_to_list(BitString)
  end.


%%========================================================================
%% get_target_log_paths(LogName) -> string()
%%
%% Gets the remote paths to log files on target.
%%========================================================================
get_target_log_paths() ->
  get_target_log_paths(?LOG_NAME).

get_target_log_paths(LogName) ->
  LogDir = get_log_dir(LogName),

  {ok, Filenames} = target_rpc(file, list_dir, [LogDir]),

  Paths = [filename:join([LogDir, Filename]) || 
           Filename <- Filenames],

  lists:usort(Paths).


%%========================================================================
%% get_target_log_contents(LogName) -> string()
%%
%% Gets the contents of log files on target.
%%========================================================================
get_target_log_contents() ->
  get_target_log_contents(?LOG_NAME).

get_target_log_contents(LogName) ->
  get_target_log_contents(LogName, false).

get_target_log_contents(LogName, MergeWithFilenames) ->
  LogPaths = get_target_log_paths(LogName),
  Contents = lists:map(fun read_target_file/1, LogPaths),

  case MergeWithFilenames of
    true ->
      FileNames = lists:map(fun filename:basename/1, LogPaths),
      lists:zip(FileNames, Contents);
    _ ->
      Contents
  end.


%%========================================================================
%% check_log(LogName, Expected, Unexpected) -> ok | {error, Results}
%%
%% Checks the contents of log files on target.
%%========================================================================
check_log(Expected) ->
  check_log(Expected, []).

check_log(Expected, Unexpected) ->
  check_log(?LOG_NAME, Expected, Unexpected).

check_log(LogName, Expected, Unexpected) ->
  CheckContent = fun(Content) ->
    check_content(Content, Expected, Unexpected)
  end,

  Contents = get_target_log_contents(LogName),
  Results  = lists:map(CheckContent, Contents),

  case is_all_true(Results) of
    true -> ok;
       _ -> {error, Results}
  end.


%% ===========================================================================
%% should_fail(M, F, A) -> ok | {Error, Result}
%%
%% Checks if faulty call to a function really causes an error.
%% ===========================================================================
should_fail(F, A) ->
  should_fail(?LOG_RAM, F, A).

should_fail(M, F, A) ->
  Res = target_rpc(M, F, A),

  case Res =/= ok of 
    true ->
      ct:log("Error in ~p:~p/~B occured, as expected:~n"
             "Response: ~p", 
             [M, F, length(A), Res]),
      
      ok;
    
    _ ->
      ct:log("Success in ~p() occured, which should have failed!~n"
              "Result:~p",
              [F, Res]),

      {error, Res}
  end.


%% ===========================================================================
%% should_fail_multi(M, F, A) -> ok | {Error, Result}
%%
%% Checks if multiple faulty calls to the same function really cause an error.
%% ===========================================================================
should_fail_multi(F, As) ->
  should_fail_multi(?LOG_RAM, F, As).

should_fail_multi(M, F, As) ->
  ShouldFail = fun(A) -> should_fail(M, F, A) end,
  Res = lists:map(ShouldFail, As),
  all_ok(Res).


%% ===========================================================================
%% COLI helper function.
%% ===========================================================================
connect() ->
    ok = rct_coli:connect(coli).

disconnect() ->
    ok = rct_coli:disconnect(coli).

send(Cmd, Expected) ->
    rct_coli:send(coli, Cmd, Expected).

connect_send(Cmd, Expected) ->
    ok = connect(),
    Res = send(Cmd, Expected),    
    ok = disconnect(),
    Res.

%%====================================================================
%% coli_exec(CmdList, Expected) -> ok | {error, Reason}
%% 
%% CmdList   = [term()] 
%% Expected  = [string()]
%%
%% Execute COLI command.
%%====================================================================
coli_exec(CmdList, Expected) ->
  Cmd = join_to_str(CmdList),
  ct:pal("=== Exec COLI command: ~p~n", [Cmd]),
  connect_send(Cmd, Expected).


%% ===========================================================================
%% ESI Log helper functions.
%% ===========================================================================

%%====================================================================
%% check_esi(Expected, Unexpected, RamLogName) -> ok | {error, Reason}
%% 
%% Expected   = [string()] 
%% Unexpected = [string()] 
%% RamLogName = string()
%%
%% Use ESI to check that the logs contain the expected entries.
%%====================================================================
check_esi(Expected, Unexpected, Config) ->
  check_esi(Expected, Unexpected, Config, ?LOG_NAME).

check_esi(Expected, Unexpected, Config, RamLogName) ->
  EsiRamLogDir = extract_esi(RamLogName, Config),
  check_esi_content(Expected, Unexpected, EsiRamLogDir).


%%====================================================================
%% check_esi_content(Expected, Unexpected, EsiRamLogDir) -> ok | {error, Reason}
%% 
%% Expected     = [string()] 
%% Unexpected   = [string()] 
%% EsiRamLogDir = string()
%%
%% Check only the contents of already extracted ESI log. Useful
%% when checking for contents of multiple RAM Logs, so only one
%% ESI Log acquisition and extraction is required.
%%====================================================================
check_esi_content(Expected, Unexpected, EsiRamLogDir) ->
    ct:pal("check_esi_content   EsiRamLogDir: ~p~n", [EsiRamLogDir]),
    GetPath = fun(FileName) -> filename:join([EsiRamLogDir, FileName]) end,
    
    CheckContent = fun(Content) -> 
			   check_content(Content, Expected, Unexpected)
		   end,
    
    {ok, LogFiles} = file:list_dir(EsiRamLogDir), 
    ct:pal("check_esi_content   LogFiles: ~p~n", [LogFiles]),
    
    case length(LogFiles) > 0 of
	true -> ok;
	_ -> ct:fail("Check ESI content ERROR: "
		     "No RAM Logs files to check.")
    end,
    
    LogPaths = lists:map(GetPath, LogFiles),
    ct:pal("check_esi_content   LogPaths: ~p~n", [LogPaths]),
    Contents = lists:map(fun read_file/1, LogPaths),
    Results  = lists:map(CheckContent, Contents),
    
    case is_all_true(Results) of
	true -> 
	    ct:pal("=== Check ESI content SUCCESSFUL ===~n"
		   "Expected found: ~p~n"
		   "Unexpected not found ~p",
		   [Expected, Unexpected]),
	    ok;
	
	_ ->
	    ct:pal("=== Check ESI content ERROR ==="),
	    {error, mismatches}
    end.

%%====================================================================
%% extract_esi(RamLogNames, Config) -> ok | {error, Reason}
%% 
%% RamLogNames = string() | [string()]
%% Config      = TC Config
%%
%% Extracts the ESI log and gets the path where
%% the RamLog files reside.
%%====================================================================
extract_esi(RamLogName, Config) ->
    {EsiLogPath, EsiLogName} = export_esi(Config),
    ct:pal("export_esi(Config) ~p~n", [{EsiLogPath, EsiLogName}]),
    os:cmd("cd " ++ EsiLogPath ++ " ; tar zxvf " ++ EsiLogName),
    ct:pal("export_esi  res  ~p~n", 
	   [filename:join(EsiLogPath, get_log_dir(RamLogName))]),
    RamLogDir = case get_rel_log_dir(RamLogName) of
		    [$/ | T] -> T;
		    T        -> T
		end,
    filename:join(EsiLogPath, RamLogDir).


%%====================================================================
%% export_esi(Config) -> ok | {error, Reason}
%% 
%% Triggers ESI log and exports it over NetConf.
%%====================================================================
export_esi(Config) ->
  logram_rpc(generate_esi),

  LogDir = get_priv_dir(Config),
  ct:pal("#### ExportedPath export_esi ~p~n", [LogDir]),

  {SftpHost, Username, Password} = get_sftp_info(),

  SFTP_URL = "sftp://" ++ Username ++ "@" ++ SftpHost,
  FullPath = SFTP_URL ++ LogDir,

  os:cmd("chmod 777 " ++ LogDir),

  Action = 
    {'ManagedElement',
      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
      [{managedElementId,[],["1"]},
       {'SystemFunctions',
        [{systemFunctionsId,[],["1"]},
         {'LogM',
          [{xmlns,"urn:com:ericsson:ecim:LogM"}],
          [{logMId,[],["1"]},
           {'exportEsi',[],
            [{uri, [], [FullPath]},
             {password, [], [Password]}]}
          ]}]}]},

  ProgressFilter = 
    {'ManagedElement',          
      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
      [{managedElementId,[],["1"]},
       {'SystemFunctions',
        [{systemFunctionsId,[],["1"]},
         {'LogM',
          [{xmlns,"urn:com:ericsson:ecim:LogM"}],
          [{logMId,[],["1"]},
           {progressReport,[],[]}
          ]}]}]},

  {ok,_} = ct_netconfc:open(nc1, []),
  Result = ct_netconfc:action(nc1, Action),

  %% Trigger the action export_esi using NetConf.
  case Result of
    {ok, _} ->
      ok;
    Error ->
      ct:pal("~p~n",[Error]),
      ct:fail("=== Error while ESI export ===")
  end,

  ct_netconfc:close_session(nc1),
  timer:sleep(500),

  {[ProgressResult], 
   [EsiLogName]} = wait_for_progress(progressReport, ProgressFilter),

  case ProgressResult of
    "SUCCESS" ->
      ct:pal("=== Successful ESI export ==="),
      check_esi_log_transfered(EsiLogName, LogDir);
    _ ->
      ct:pal("=== Error in ESI export ===~n~p~n",[ProgressResult]),
      ct:fail({ProgressResult, EsiLogName})
  end.


%% @hidden
%%--------------------------------------------------------------------
%% Will check that esi log is tranfered to expected path
%% and also the size of the file. If the size is too small,
%% then maybe it does not consist on anything!
%%--------------------------------------------------------------------
check_esi_log_transfered(EsiLogName, EsiLogPath) ->
  ct:pal("#### EsiLogPath check_esi_log_transfered ~p~n", 
         [EsiLogPath]),

  {SftpHost, Username, Password} = get_sftp_info(),

  SftpOpts = [{user,                  Username},
              {password,              Password},
              {silently_accept_hosts, true},
              {timeout,               10000}],

  {ok, ChPid, _ChRef} = ssh_sftp:start_channel(SftpHost, SftpOpts),
  {ok, LogFiles}      = ssh_sftp:list_dir(ChPid, EsiLogPath, 2000),
  IsEsiLog            = fun(Str) -> Str == EsiLogName end,
  DoesEsiLogExist     = lists:any(IsEsiLog, LogFiles),
  EsiLogFullPath      = EsiLogPath ++ EsiLogName,

  case DoesEsiLogExist of
    true -> 
      ct:pal("### Esi log file: ~p, exists in \n path : ~p \n",
      [EsiLogName, EsiLogPath]);

    false -> 
      ct:fail(" Could not find the ESI log file on SFTP server.")
  end,
     
  %%==============================================================
  %% After having checked that the ESI log
  %% file exists, we need to check its size.
  %%==============================================================
  {ok, FileInfo} = ssh_sftp:read_file_info(ChPid, EsiLogFullPath, 2000),
  Size           = lists:nth(2, tuple_to_list(FileInfo)),

  ct:pal("### Recieved FileInfo: ~p", [FileInfo]),

  if 
    Size > 10000 ->
      ct:pal("### Recieved Size: ~p", [Size]),
      true;

    true ->
      ct:pal("### Size of the esi tar file is: ~p. ~n "
       "It is smaller than expected. ~n "
       "Unpack the file and ckeck that it look OK. \n", [Size]),
      ct:fail("Size of the esi log file is too small! check if it "
        "looks ok after unpack!.")
  end,  

  StopResult = ssh_sftp:stop_channel(ChPid),
  ct:pal("### stop sftp channel ~p", [StopResult]),

  {EsiLogPath, EsiLogName}.


%%-------------------------------------------------------------------
%% get_sftp_info() -> ok | {SftpHost, Username, Password}
%% 
%% Gets the info for connecting to SFTP host.
%%-------------------------------------------------------------------
get_sftp_info() ->
  HostInfo = ct:get_config(sftp_server),

  SftpHost = proplists:get_value(host,     HostInfo),
  Username = proplists:get_value(username, HostInfo),
  Password = proplists:get_value(password, HostInfo),

  {SftpHost, Username, Password}.

%%-------------------------------------------------------------------
%% Loops until the progress information says FINISHED.
%% Requires a name and a NetConf filter for extracting
%% the progress report attribute.
%%-------------------------------------------------------------------
wait_for_progress(Attribute, ProgressFilter) ->
  {ok, _} = ct_netconfc:open(nc1, []),
  {ok, A} = ct_netconfc:get(nc1, ProgressFilter),

  ct_netconfc:close_session(nc1),

  {ok, Report} = extract_element(Attribute, A),
  {ok, State}  = extract_element(state, [Report]),

  timer:sleep(1000),

  case State of
    {state, _, [Verdict]} 
      when Verdict == "FINISHED";
           Verdict == "CANCELLED" ->
      {ok, {result,     _, Result}}     = extract_element(result,     [Report]),
      {ok, {resultInfo, _, ResultInfo}} = extract_element(resultInfo, [Report]),
      {Result, ResultInfo};

    {state, _, [_Verdict]} ->
        wait_for_progress(Attribute, ProgressFilter)
  end.


%%-------------------------------------------------------------------
%% From a general short XML syntax, extract a certain XML element.
%%-------------------------------------------------------------------
extract_element(Element, [{Element, Attribute, Content} | _]) ->
  {ok, {Element, Attribute, Content}};

extract_element(Element, [{Element, Content} | _]) ->
  {ok, {Element, Content}};

extract_element(Element, [{_, _, Content} | Elements]) ->
    extract_element(Element, [{undefined, Content} | Elements]);

extract_element(Element, [{_, Content} | Elements]) ->
  case extract_element(Element, Content) of
    {ok, Value} -> {ok, Value};
    not_found   -> extract_element(Element, Elements)
  end;

extract_element(Element, [_ | Elements]) ->
  extract_element(Element, Elements);

extract_element(_, []) ->
  not_found.


%%====================================================================
%% logs_contains_data(Data, FormattedLog) -> ok | {error, not_found}
%%
%% Data = string()
%% FormattedLogs = [Entry]
%% Entry = #{
%%            counter   => string(),
%%            date      => string(),
%%            time      => string(),
%%            pid       => string(),
%%            mod       => string(),
%%            options   => string(),
%%            data      => string()
%%          }
%%
%% Checks if specific log contains Data.
%%====================================================================
log_contains_data(_Data, []) ->
  {error, not_found};

log_contains_data(Data, [Entry|Tail]) ->
  case string:str(maps:get(data, Entry), Data) =/= 0 of
    true ->
      ok;
    _ ->
      log_contains_data(Data, Tail)
  end.

log_contains_data([], Key, [Entry|Tail]) ->
  Value = maps:get(Key, Entry),
  case Value == [] of
    true ->
      ok;
    _ ->
      log_contains_data([], Key, Tail)
  end;
  
log_contains_data(Data, Key, [Entry|Tail]) ->
  Value = maps:get(Key, Entry),
  case string:str(Value, Data) =/= 0 of
    true ->
      ok;
    _ ->
      log_contains_data(Data, Key, Tail)
  end.


%%====================================================================
%% get_logs_formated(Content) -> [Entry]
%% 
%% Content = string()
%% Entry = #{
%%            counter   => string(),
%%            date      => string(),
%%            time      => string(),
%%            pid       => string(),
%%            mod       => string(),
%%            options   => string(),
%%            data      => string()
%%          }
%%
%% Extracts and formats log data into list of entries.
%%====================================================================
format_log_content(Content) ->
  %% options for parsing
  Options = [global,
             {capture, all, list}],

  %% regex definitions
  PrefixRE    = "\\.*",
  CounterRE   = "(\\d*)[ ]?",
  TimestampRE = "([\\d]{4}-[\\d]{2}-[\\d]{2})T" ++ 
                "([\\d]{2}:[\\d]{2}:[\\d]{2}\\.[\\d]{3})Z",
  PidRE       = "(<\\d+\\.\\d+\\.\\d+>)",
  ModuleRE    = "([^:]+):[\\d]+",
  OptsRe      = "([^\\n]*)",
  
  %% join all header regex
  HeaderRE    = 
    PrefixRE ++ 
    CounterRE ++
    TimestampRE ++ " " ++
    PidRE ++ " " ++
    ModuleRE ++ "[ ]?" ++
    OptsRe,

  %% log data regex
  LogDataRE   = "(.*)",

  %% list all logs in file
  {match, Matches} = re:run(
    Content,
    HeaderRE ++ "\\n" ++ LogDataRE,
    Options),
  logslist_to_map(Matches).

%%-------------------------------------------------------------------
%% Helper function to convert LogList to list of maps.
%% 
%% LogList = [[string()]]
%%-------------------------------------------------------------------
logslist_to_map([], Acc) ->
  Acc;

logslist_to_map([Entry|Tail], Acc) ->
  [
    _AllContent,
    Counter,
    Date,
    Time,
    Pid,
    Module,
    Options,
    Log
  ] = Entry,
  Map = #{
    %% all       => AllContent,
    counter   => Counter,
    date      => Date,
    time      => Time,
    pid       => Pid,
    mod       => Module,
    options   => Options,
    data      => Log
  },
  logslist_to_map(Tail, [Map|Acc]).

logslist_to_map(LogList) ->
  lists:reverse(logslist_to_map(LogList, [])).
  

%%====================================================================
%% get_log_formatted(Config) -> [LogInfo]
%%
%% LogInfo = {FileName, [LogEntry]},
%% FileName = string(),
%% LogEntry = #{
%%                counter   => string(),
%%                date      => string(),
%%                time      => string(),
%%                pid       => string(),
%%                mod       => string(),
%%                options   => string(),
%%                data      => string()
%%              }
%%
%% Use ESI to get the logs.
%%====================================================================
get_log_formatted() ->
  get_log_formatted(?LOG_NAME).

get_log_formatted(RamLogName) ->  
  ConvertToMap = fun({FileName, FileContent}) ->
    FormattedData = format_log_content(FileContent),
    {FileName, FormattedData}
  end,

  LogData = get_target_log_contents(RamLogName, true),
  lists:map(ConvertToMap, LogData).

  
%%====================================================================
%% get_esi_content(EsiRamLogDir) -> [{FileName, FileContent}]
%% 
%% EsiRamLogDir = string(),
%% FileName = string(),
%% FileContents = string()
%%
%% gets content of all log files
%%====================================================================
get_esi_content(EsiRamLogDir) ->
  GetPath = fun(FileName) -> 
    filename:join([EsiRamLogDir, FileName])
  end,

  {ok, LogFiles} = file:list_dir(EsiRamLogDir), 
  LogPaths       = lists:map(GetPath, LogFiles),
  Contents       = lists:map(fun read_file/1, LogPaths),
    
  lists:zip(LogPaths, Contents).


%% ===========================================================================
%% Node restart helper functions.
%% ===========================================================================

%%========================================================================
%% restart_node() -> ok | {error, reason}
%%
%% Cold restarts the node, and block further execution
%% until it is back up.
%%========================================================================
restart_node(Timeout) ->
  ct:pal("=== Restarting node (cold restart)... ===~n"
         "Timeout: ~.1f min",
         [Timeout / 60 / 1000]),

  ok = target_rpc(init, reboot),
  wait_until_node_up(Timeout).


%%========================================================================
%% wait_node_up(Timeout, ShouldStartMonitor) -> ok | {error, reason}
%%
%% Timeout            = integer()
%% ShouldStartMonitor = bool()
%%
%% Wait until the node has restarted. The availability of the
%% node is checked by trying to connect to NetConf.
%%========================================================================
wait_until_node_up(Timeout) ->
  wait_until_node_up(Timeout, true).

wait_until_node_up(Timeout, ShouldStartMonitor) ->
  Start = get_timestamp(),
  Node = target_rpc(erlang, node),

  case ShouldStartMonitor of
    true  -> erlang:monitor_node(Node, true);
    false -> ok
  end,

  receive
    {nodedown, ReceivedNode} ->
      ct:pal("=== Node ~p shut down. Waiting to start up... ===", [ReceivedNode]),
      erlang:monitor_node(ReceivedNode, false),
      Remaining = reduce_duration(Timeout, Start),
      ping_node_until_up(Remaining);

    Unhandled ->
       ct:pal("=== Unhandled message while waiting node restart === ~n"
              "ReceivedInfo: ~p ", 
              [Unhandled]),
      
      wait_until_node_up(Timeout, false)

  after Timeout ->
    ct:pal("=== Timeout while waiting node restart ==="
           "Timeout was: ~p ms",
           [Timeout]),
    
    erlang:monitor_node(Node, false),
    {error, restart_timeout}
  end.


%%========================================================================
%% ping_node_until_up(Interval, Timeout) -> ok | {error, reason}
%%
%% Interval = integer()
%% Timeout  = integer()
%%
%% Ping the node in given interval until it is running again or
%% timeout occurs. Whether the node is operational or not is
%% determened by the ability to connect to NetConf.
%%========================================================================
ping_node_until_up(Timeout) ->
  ping_node_until_up(Timeout, ?NODE_PING_INTERVAL).

ping_node_until_up(Timeout, _Interval) when Timeout =< 0 ->
  {error, node_startup_timeout};

ping_node_until_up(Timeout, Interval) ->
  Start = get_timestamp(),

  case check_netconf() of
    ok -> 
      ct:pal("Node has restarted!", []),
      ok;
    {error, Reason } ->
      ct:log("Node still hasn't restarted.~n"
             "Reason: ~p~n"
             "Re-pinging the node in ~.1f sec...",
             [Reason, Interval / ?SECOND]),

      timer:sleep(Interval),
      Remaining = reduce_duration(Timeout, Start),
      ping_node_until_up(Remaining, Interval)
  end.


%%========================================================================
%% check_netconf() -> ok | {error, reason}
%%
%% Checks whether NetConf is operational on node.
%%========================================================================
check_netconf() ->
  Res = ct_netconfc:open(nc1, []),

  case Res of
    {ok, _Handle} -> 
      ct_netconfc:close_session(nc1),
      ok;
    _Failure ->
      {error, _Failure}
  end.


%%========================================================================
%% get_timestamp() -> integer()
%%
%% Gets the current timestamp in milliseconds.
%%========================================================================
get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).


%%========================================================================
%% get_duration_since(Start) -> Duration()
%%
%% Start    - milliseconds() 
%% Duration - milliseconds() 
%%
%% Gets the time passed sinde the provided time.
%%========================================================================
get_duration_since(Start) ->
  End = get_timestamp(),
  End - Start.

%%========================================================================
%% reduce_duration(Duration, Start) -> integer()
%%
%% Timeout      - milliseconds()
%% Start        - milliseconds()
%% Milliseconds - integer()
%%
%% Reduce the given duration (provided in ms) by the time
%% that has passed since the given start (also in ms), until now.
%%========================================================================
reduce_duration(Duration, Start) ->
  RunningTime = get_duration_since(Start),
  Duration - RunningTime.

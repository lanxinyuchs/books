%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	log_ram_basic_SUITE.erl %
%%% @author eolaand
%%% @copyright Ericsson AB 2016
%%% @version /main/R5A/R8A/1
%%%
%%% @doc == Test Suite for testing application logs.==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% @end

-module(log_ram_basic_SUITE).

%%% ----------------------------------------------------------
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016 All rights reserved.
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
%%% R5V/1      2016-03-07 edamkon     Created
%%% ----------------------------------------------------------
%%%

-include_lib("common_test/include/ct.hrl").
-include("log_ram_test.hrl").

-export([
  suite/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/2,
  all/0,
  groups/0
]).

-export([
  blacklist_file/1,
  blacklist_timeout/1,
  change_filter_level/1,
  check_max_log_size/1,
  fetch_esi/1,
  multiple_files/1,
  options_filesize/1,
  options_header/1,
  options_seq_no/1,
  options_zip/1,
  set_filter_level/1,
  set_filter_level_coli/1,
  two_log_interference/1,
  write_log_basic/1
]).

-define(LIB, log_ram_test_lib).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf.
%% @end
%%--------------------------------------------------------------------
suite() ->
  [{ct_hooks, [
     {rct_rpc,     rpc_1},
     {rct_netconf, {nc1, html}},
     {rct_coli,    {coli, [manual_connect]}}
  ]}].

%% @hidden
init_per_suite(Config) ->
  Config.

%% @hidden
end_per_suite(_Config) ->
  ok.

%% @hidden
init_per_testcase(two_log_interference, Config) ->
  delete_logs([?LOG_NAME_1, ?LOG_NAME_2]),
  Config;

%% @hidden
init_per_testcase(_TestCase, Config) ->
  delete_logs(),
  Config.

%% @hidden
end_per_testcase(two_log_interference, Config) ->
  print_ram_log_files(Config, [?LOG_NAME_1,
                               ?LOG_NAME_2]),

  cleanup(Config, [?LOG_NAME_1,
                   ?LOG_NAME_2]);

%% @hidden
end_per_testcase(_TestCase, Config) ->
  print_ram_log_files(Config),
  cleanup(Config).

%% @hidden
groups() ->
  [{default__group, [], all()}].

all() -> [
  blacklist_file,
  blacklist_timeout,
  change_filter_level,
  check_max_log_size,
  fetch_esi,
  multiple_files,
  options_filesize,
  options_header,
  options_seq_no,
  options_zip,
  set_filter_level,
  set_filter_level_coli,
  two_log_interference,
  write_log_basic
].


%% ===========================================================================
%% TEST CASES
%% ===========================================================================

%%--------------------------------------------------------------------
%% @doc
%% Test basic write_log functionality
%% @end
%%--------------------------------------------------------------------
write_log_basic(_Config) ->
  TestText    = "Sample message",
  ErrorText   = "Error message",
  WarningText = "Warning message",

  TestData    = {1,       TestText,    false},
  TestError   = {error,   ErrorText,   false},
  TestWarning = {warning, WarningText, false},

  TestTag  = {?MODULE, 370, self()},

  FaultySeverityData  = {faulty_severity, "Data",      false},
  FaultyTextData      = {1,               faulty_text, false},
  FaultyBlacklistData = {1,               "Data",      faulty_BL},

  ok = create_log([{zip, false}]),

  %% ---------------------------------------------
  %% Try doing faulty write_log() calls.
  %% ---------------------------------------------
  ok = should_fail_multi(write_log,
                        [[faulty_log, TestTag,    TestData],
                         [?LOG_NAME,  faulty_tag, TestData],
                         [?LOG_NAME,  TestTag,    FaultyTextData],
                         [?LOG_NAME,  TestTag,    FaultySeverityData],
                         [?LOG_NAME,  TestTag,    FaultyBlacklistData]]),
  
  %% ---------------------------------------------
  %% Try doing proper write_log() call.
  %% ---------------------------------------------
  ok = write_log_multi([TestData, TestError, TestWarning]),

  %% ---------------------------------------------
  %% Write to file and check it's contents.
  %% ---------------------------------------------
  ok = write_to_file(),
  ok = check_log([TestText, ErrorText, WarningText]).


%%--------------------------------------------------------------------
%% @doc
%% Set filter level and see that a write_log
%% is either discarded or written to the log.
%% @end
%%--------------------------------------------------------------------
set_filter_level(_Config) ->
  ok = create_log([{zip, false}, 
                   {maxNoFiles, 3}]),

  %% ---------------------------------------------
  %% Try doing faulty set_severity().
  %% ---------------------------------------------
  ok = should_fail(set_severity, [?LOG_NAME, -1]),
  ok = should_fail(set_severity, [?LOG_NAME, 10]),

  %% ---------------------------------------------
  %% Try doing proper set_severity() and check
  %% the results.
  %% ---------------------------------------------
  ok = set_severity(5),

  ok = write_log_multi([{1, "severity1"},
                        {2, "severity2"},
                        {5, "severity5"},
                        {6, "severity6"},
                        {7, "severity7"}]),

  ok = write_to_file(),
  ok = check_log(["severity1", "severity2", "severity5"],
                 ["severity6", "severity7"]).


%%--------------------------------------------------------------------
%% @doc
%% Set filter level and see that a write_log
%% is either discarded or written to the log.
%% @end
%%--------------------------------------------------------------------
set_filter_level_coli(_Config) ->
  ok = create_log([{zip, false}, 
                   {maxNoFiles, 3}]),

  %% ---------------------------------------------
  %% Try doing faulty set_severity_coli().
  %% ---------------------------------------------
  {error, _} = coli_exec(["/log/ramlog", faulty_log, 5],   "ok"),
  {error, _} = coli_exec(["/log/ramlog", ?LOG_NAME,  10],  "ok"),
  {error, _} = coli_exec(["/log/ramlog", ?LOG_NAME,  "a"], "ok"),
  {error, _} = coli_exec(["/log/ramlog"],                  "ok"),

  %% ---------------------------------------------
  %% Do a proper set_severity_coli(), and
  %% check the results.
  %% ---------------------------------------------
  {ok, _} = coli_exec(["/log/ramlog", ?LOG_NAME, 5], "ok"),

  ok = write_log_multi([{1, "severity1"},
                        {2, "severity2"},
                        {5, "severity5"},
                        {6, "severity6"},
                        {7, "severity7"}]),

  ok = write_to_file(),

  ok = check_log(["severity1", "severity2", "severity5"],
                 ["severity6", "severity7"]).


%%--------------------------------------------------------------------
%% @doc
%% Set filter level and write to log. Change the filter level and 
%% see that the proper messages are written to the log.
%% @end
%%--------------------------------------------------------------------
change_filter_level(_Config) ->
  ok = create_log([{zip, false}, 
                   {maxNoFiles, 3}]),

  ok = set_severity(7),

  ok = write_log_multi([{4, "severity4"},
                        {6, "severity6"}]),

  ok = set_severity(3),

  ok = write_log_multi([{4, "severity4-again"},
                        {6, "severity6-again"}]),

  ok = write_to_file(),

  ok = check_log(["severity4", "severity6"], 
                 ["severity4-again", "severity6-again"]).


%%--------------------------------------------------------------------
%% @doc
%% Create 2 different RAM logs.
%% Write into both.
%% Check that logs do not interfere.
%% @end
%%--------------------------------------------------------------------
two_log_interference(_Config) ->
  Log1 = ?LOG_NAME_1,
  Log2 = ?LOG_NAME_2,

  ok = create_log(Log1, [{zip, false}, {maxNoFiles, 3}]),
  ok = create_log(Log2, [{zip, false}, {maxNoFiles, 3}]),

  ok = write_log(Log1, [{1, "Log1Message"}]),
  ok = write_log(Log2, [{1, "Log2Message"}]),

  ok = write_to_file(Log1),
  ok = write_to_file(Log2),

  check_log(Log1, ["Log1"], ["Log2"]),
  check_log(Log2, ["Log2"], ["Log1"]).
  

%%--------------------------------------------------------------------
%% @doc
%% Set file size and write multiple data to log.
%% Check if there are 3 files generated.
%% @end
%%--------------------------------------------------------------------
multiple_files(_Config) ->
  ok = create_log([{zip, false}, 
                   {maxNoFiles, 3},
                   {size, 1}]),
  %% write multiple logs
  ok = write_log_n([{1, "something"}], 4000),
  ok = write_to_file(),
  
  %% get list of files
  LogFiles = get_target_log_paths(),
  
  %% check number of files
  3 = length(LogFiles),
  ok = check_log(["something"]).


%%--------------------------------------------------------------------
%% @doc
%% Check that the combined size of RAM Log files does not exceed 6 MB.
%% Also checks if more than 6 files are generated, or if individual
%% filesize is over 1MB.
%% @end
%%--------------------------------------------------------------------
check_max_log_size(_Config) ->
  MaxAllowedSize =  6 * 1000 * 1000, %%  6MB
  Tolerance      = 10 * 1024,        %% 10KB

  AllowedSpec  = [{size,       1000},
                  {zip,        false}, 
                  {maxNoFiles, 6}],

  InvalidSpec1 = [{size,       6000},
                  {zip,        false}, 
                  {maxNoFiles, 1}],

  InvalidSpec2 = [{size,       1000},
                  {zip,        false}, 
                  {maxNoFiles, 10}],

  %% ---------------------------------------------
  %% Reserving a portion of the RAM more than 1MB
  %% in size should fail.
  %% ---------------------------------------------
  {error, _Message1} = create_log(InvalidSpec1),

  %% ---------------------------------------------
  %% Allowing for creating more than 6MB worth of
  %% RAM Log files should also fail.
  %% ---------------------------------------------
  {error, _Message2} = create_log(InvalidSpec2),
  
  %% ---------------------------------------------
  %% Creating several files under 1MB in size
  %% should pass.
  %% ---------------------------------------------
  ok = create_log(AllowedSpec),

  %% ---------------------------------------------
  %% Write a lot more than 6MB worth of entries in
  %% RAM Log.
  %% ---------------------------------------------
  ok = write_log_n([{1, "something"}], 100000),
  ok = write_to_file(),

  %% ---------------------------------------------
  %% Fetch ESI and check if the combined filesize
  %% of all generated RAM Log files is 6MB or less.
  %% ---------------------------------------------
  LogFiles = get_target_log_paths(),

  GetFileSize = fun(Path) ->
    {ok, Info} = target_rpc(file, read_file_info, [Path]),
    Info#file_info.size
  end,

  FileSizes        = lists:map(GetFileSize, LogFiles),
  CombinedSize     = lists:sum(FileSizes),

  true = CombinedSize =< MaxAllowedSize + Tolerance.


%%--------------------------------------------------------------------
%% @doc
%% Test blacklist file flag
%% @end
%%--------------------------------------------------------------------
blacklist_file(_Config) ->
  ok = create_log([{zip, false}, 
                   {maxNoFiles, 3}]),
  
  %% file blacklist test
  Tag1 = {?MODULE, 1, self()},
  Tag2 = {?MODULE, 2, self()},
  
  %% write with blacklist 'file'
  ok = write_log({1, "expected1", file}, Tag1),

  %% try to write blacklistes log, whould not be written
  ok = write_log({1, "error1", file}, Tag1),
  
  %% write with the different tag, should be written
  ok = write_log({1, "expected2", file}, Tag2),

  ok = write_to_file(),
  ok = check_log(["expected1", "expected2"], ["error1"]).

%%--------------------------------------------------------------------
%% @doc
%% Test blacklist timeout (cooldown) flag
%% @end
%%--------------------------------------------------------------------
blacklist_timeout(_Config) ->
  ok = create_log([{zip, false}, 
                   {maxNoFiles, 3}]),

  Tag1 = {?MODULE, 1, self()},
  Tag2 = {?MODULE, 2, self()},
  
  %% write with blacklist timeout
  ok = write_log({1, "expected1", 5}, Tag1),
  %% try to write blacklistes log, whould not be written
  ok = write_log({1, "error1", 5}, Tag1),
  
  %% write with the different tag, should be written
  ok = write_log({1, "expected2", 5}, Tag2),

  ok = write_to_file(),
  ok = check_log(["expected1", "expected2"], ["error1"]).


%%--------------------------------------------------------------------
%% @doc
%% Test setting the zip option.
%% @end
%%--------------------------------------------------------------------
options_zip(Config) ->
  ok = create_log([{zip, false}]),
  ok = write_log({1, "before-zip"}),

  ok = set_options([{zip, true}]),
  ok = write_log({1, "after-zip"}),

  ok = write_to_file(),

  ArchiveExtension    = ".gz",
  LocalDir            = get_priv_dir(Config),
  [TargetLogFilePath] = get_target_log_paths(),
  ArchiveBinary       = read_target_file(TargetLogFilePath, true),
  LogFileName         = filename:basename(TargetLogFilePath),
  LocalArchivePath    = filename:join([LocalDir, LogFileName]),
  
  true = filename:extension(LogFileName) == ArchiveExtension,
  ok   = file:write_file(LocalArchivePath, ArchiveBinary),
  
  ExtractedName = filename:basename(LogFileName, ArchiveExtension),
  ExtractedPath = filename:join(LocalDir, ExtractedName),
  ExtractCmd    = io_lib:format("gunzip -c ~p > ~p", 
                                [LocalArchivePath, ExtractedPath]),

  os:cmd(ExtractCmd),

  Content = read_file(ExtractedPath),
  check_content(Content, ["before-zip", "after-zip"]).


%%--------------------------------------------------------------------
%% @doc
%% Test setting the maxNoFiles and size options
%% @end
%%--------------------------------------------------------------------
options_filesize(_Config) ->
  ok = create_log([{zip, false}]),
  
  ok = set_options([
                    {zip, false},
                    {size, 1},
                    {maxNoFiles, 3}
                  ]),
  
  ok = write_log_n([{1, "something"}], 4000),
  [_1, _2, _3] = get_target_log_paths(),
  ok = check_log(["something"]).


%%--------------------------------------------------------------------
%% @doc
%% Test setting the header function option.
%% @end
%%--------------------------------------------------------------------
options_header(Config) ->
  ok = create_log(),

  %% ---------------------------------------------
  %% Test setting header as a {M, F, A} tuple.
  %% ---------------------------------------------
  MFA = {string, join, [["header", "test"], " "]},
  ok = set_options([{zip, false}, {header, MFA}]),
  ok = write_log({1, "first"}),

  ok = write_to_file(),
  ok = check_log(["first", "header test"]),

  %% ---------------------------------------------
  %% Write the log to a file, to preserve the
  %% first header. It would otherwise be
  %% overwritten by the second one.
  %% ---------------------------------------------
  print_ram_log_files(Config),
  cleanup(Config),
  ok = create_log(),

  %%----------------------------------------------
  %% Test setting header as a fun.
  %% ---------------------------------------------
  HeaderFun   = fun() -> "custom_header_fun" end,
  {M, Bin, F} = code:get_object_code(?MODULE),
  {module, M} = target_rpc(code, load_binary, [M, F, Bin], true),
  _Pid        = target_rpc(erlang, spawn, [HeaderFun]),

  ok = set_options([{zip, false}, {header, HeaderFun}]),
  ok = write_log({1, "second"}),

  ok = write_to_file(),
  ok = check_log(["second", "custom_header_fun"]).


%%--------------------------------------------------------------------
%% @doc
%% Test setting the seqNo option
%% @end
%%--------------------------------------------------------------------
options_seq_no(_Config) ->
  ok = create_log(),

  ok = write_log({1, "before"}),
  
  ok = set_options([{zip, false},
                    {seqNo, false}]),

  ok = write_log({1, "after"}),

  ok = set_options([{zip, false},
                    {seqNo, true}]),
  
  ok = write_log({1, "after2"}),

  ok = write_to_file(),
  ok = check_log(["before", "after", "after2"]),

  [{_Filename, FormattedData}] = get_log_formatted(),

  ct:pal("FormattedData : ~p", [FormattedData]),

  ok = log_contains_data([],  counter, FormattedData),
  ok = log_contains_data("1", counter, FormattedData).


%%--------------------------------------------------------------------
%% @doc
%% - Write to the RAM Log
%% - Get the file contents by fetching ESI log
%% @end
%%--------------------------------------------------------------------
fetch_esi(Config) ->
  LogMessage = "Check log by fetching ESI.",

  ok = create_log([{zip, false}]),
  ok = write_log({1, LogMessage}),
  ok = check_esi([LogMessage], [], Config).


%% ===========================================================================
%% HELPER FUNCTION ALIASES
%% ===========================================================================

create_log()                   -> ?LIB:create_log().
create_log(A)                  -> ?LIB:create_log(A).
create_log(A, B)               -> ?LIB:create_log(A, B).
write_log(A)                   -> ?LIB:write_log(A).
write_log(A, B)                -> ?LIB:write_log(A, B).
write_log_n(A, B)              -> ?LIB:write_log_n(A, B).
write_log_multi(A)             -> ?LIB:write_log_multi(A).
set_severity(A)                -> ?LIB:set_severity(A).
delete_logs()                  -> ?LIB:delete_logs().
delete_logs(A)                 -> ?LIB:delete_logs(A).
cleanup(A)                     -> ?LIB:cleanup(A).
cleanup(A, B)                  -> ?LIB:cleanup(A, B).
print_ram_log_files(A)         -> ?LIB:print_ram_log_files(A).
print_ram_log_files(A, B)      -> ?LIB:print_ram_log_files(A, B).
coli_exec(A, B)                -> ?LIB:coli_exec(A, B).
write_to_file()                -> ?LIB:write_to_file().
write_to_file(A)               -> ?LIB:write_to_file(A).
check_esi(A, B, C)             -> ?LIB:check_esi(A, B, C).
check_log(A)                   -> ?LIB:check_log(A).
check_log(A, B)                -> ?LIB:check_log(A, B).
check_log(A, B, C)             -> ?LIB:check_log(A, B, C).
get_target_log_paths()         -> ?LIB:get_target_log_paths().
target_rpc(A, B, C)            -> ?LIB:target_rpc(A, B, C).
target_rpc(A, B, C, D)         -> ?LIB:target_rpc(A, B, C, D).
should_fail(A, B)              -> ?LIB:should_fail(A, B).
should_fail_multi(A, B)        -> ?LIB:should_fail_multi(A, B).
set_options(A)                 -> ?LIB:set_options(A).
read_file(A)                   -> ?LIB:read_file(A).
get_log_formatted()            -> ?LIB:get_log_formatted().
get_priv_dir(A)                -> ?LIB:get_priv_dir(A).
read_target_file(A, B)         -> ?LIB:read_target_file(A, B).
check_content(A, B)            -> ?LIB:check_content(A, B).
log_contains_data(A, B, C)     -> ?LIB:log_contains_data(A, B, C).

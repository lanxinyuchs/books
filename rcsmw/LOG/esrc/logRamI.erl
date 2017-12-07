%%% #---------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	logRamI.erl %
%%% @author uabesvi
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R5A/R6A/R8A/6
%%%
%%% @doc 
%%% This module contains the interface for the RAM log service.
%%% The purpose of the RAM log is to minimize the disc usage. 
%%%
%%% === Attributes ===
%%% A RAM log has the following attributes:<br/>
%%% <ul>
%%%  <li>size of the used RAM specified in kB (default: 1000 kB)</li>
%%%  <li>number of files to be stored (default: 3)</li>
%%%  <li>add a header to each file (optional)</li>
%%%  <li>add a sequence number to each log entry (default: true)</li>
%%%  <li>compress the files when the log is written on disc (default: true)</li>
%%% </ul>
%%% 
%%% It is also possible, at a node start, to modify the attributes 
%%% from a configuration file, refer to create_log/2 below.
%%% 
%%% === Disc usage ===
%%% RAM log entries are stored in RAM until:
%%% <ul>
%%%  <li>the requested storage size is reached</li>
%%%  <li>the log is deleted</li>
%%%  <li>ESI is requested</li>
%%%  <li>system restart</li>
%%% </ul>
%%% when the entries are written to a file on disc.
%%% The files are stored in the following directory <i>/rcs/log/LogName</i>
%%%
%%% === Filtering of log entries ===
%%% Each log entry is tagged with a severity level. It is possible
%%% to filter log entries by setting a severity log level per log.
%%% Only log entries tagged with a level lower or equal to the current 
%%% filter level will be written to the log.
%%% The default severity filter level is 1 (one).
%%% 
%%% The following coli command can be used to change the severity level:
%%% ```
%%%   /log/ramlog LogName SeverityFilterLevel
%%% '''
%%%
%%% === Black list ===
%%% It is possible to inhibit similar log entries to be logged multiple times 
%%% by putting them into a black list either for a certain
%%% time or by indicating that similar log entries should only be written
%%% once per file.
%%% 
%%% @end 
%%% 

-module(logRamI).
-vsn('/main/R5A/R6A/R8A/6').
-date('2017-01-10').
-author('uabesvi').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% R5A/1-4    2016-02-18 uabesvi     Created
%%% R5A/5      2016-02-22 etxtory     Fix
%%% R5A/6-11   2016-02-24 uabesvi     Documentation
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([create_log/2]).
-export([delete_log/1]).
-export([write_log/3]).

-export([generate_esi/0]).

%% COLI callback
-export([coli_set_severity/1]).

%% Test functions
-export([write_to_file/0]).
-export([write_to_file/1]).

-export([set_severity/2]).
-export([set_options/2]).
-export([print_loop_data/0]).
-export([print_loop_data/1]).
-export([print_logs/0]).
-export([print_logs/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc
%%% Create a RAM log with the specified configuration.
%%% === Arguments ===
%%% Name
%%% ```
%%% Name of the RAM log.
%%% The name is used both as the directory name and as the file name, when stored on disc.
%%% The files are stored in directory /rcs/log/Name. The files are called Name_Timestamp.txt.gz 
%%% where Timestamp is an integer.
%%% '''
%%%
%%% Options : [Option]
%%% ```
%%%    The configuration options.
%%% '''
%%%
%%% Option : {size, Size} | {maxNoFiles, MaxNoFiles} | {header, Header} |
%%%          {zip, Zip} | {seqNo, SeqNo} | {line, Line}
%%%
%%% Size
%%% ```
%%% Size of the RAM where the log entries are stored before they are written to disc.
%%% The size is defined in kB.
%%%
%%% NOTE: The total size, i.e. size * maxNoFiles is limited to 6 MB
%%% '''
%%% MaxNoFiles 
%%% ```
%%% Max number of files stored on disc. 
%%% '''
%%% Header
%%% ```
%%% If fun() or MFA is specified the reply of the function is written first in each file.
%%% '''
%%%
%%% Zip 
%%% ```
%%% Specifies if the files are to be compressed when written to disc. Default is true.
%%% '''
%%% SeqNo
%%% ```
%%% Specifies if each entry will be prefixed with a sequence number. Default is true.
%%% '''
%%% Line
%%% ```
%%% Specifies if the line number from where the write_log is invoked will 
%%% be printed in the entry's heading. Default is true.
%%% '''
%%% === Log Entry Example ===
%%% A log entry will be stored in the following format:
%%% ```
%%% SeqNo DateTime Pid Module[:Line] S:Severity [BT:BlackListTag]
%%% Message
%%% 
%%% S:  is the prefix for the severity level.
%%% BL: if present, indicates that the entry was black listed and with what tag.
%%%
%%% Example of an log entry:
%%%
%%% .........1 2016-02-23T16:38:54.032Z <0.3786.0> pmsServer:275 S:1 BL:file
%%% started
%%% '''
%%% === Options configuration file ===
%%% It is possible to modify a log's options at node start by defining
%%% the following configuration file in dev_patches
%%%
%%% ```
%%%   .../dev_patches/ramlog.cmds
%%% '''
%%% To modify a log's options use the following syntax: 
%%%
%%% ```
%%% {Name, [ArgOption | {severity, Severity}]}.
%%% 
%%% Name      - name of the RAM log 
%%% ArgOption - as defined in Options list in the Arguments above 
%%% Severity  - 0..9 
%%%             set the severity filter level for a log.
%%%             Severity has the same effect as the coli command /log/ramlog  
%%% '''
%%% 
%%%
%%% @end
%%% ----------------------------------------------------------

-spec create_log(Name, Options) -> ok | {error, Reason} when
    Name :: string(),
    Options :: [Option],
    Option :: {size, Size :: integer()} | 
              {maxNoFiles, MaxNoFiles :: integer()} | 
              {header, Header} |
              {zip, Zip :: boolean()} | 
              {seqNo, SeqNo :: boolean()} | 
	      {line, Line :: boolean()},
    Header :: fun() | {Module :: atom(), Function :: atom(), Args},
    Args :: [term()],
    Reason :: term().



create_log(Name, Options) when is_atom(Name) ->
    create_log(atom_to_list(Name), Options);
create_log(Name, Options) ->
    logRamServer:create_log(Name, Options).



%%% ----------------------------------------------------------
%%% @doc
%%% Delete a RAM log.
%%%
%%% === Arguments ===
%%% Name
%%% ```
%%% Name of the RAM log
%%% '''
%%% NOTE: No files are deleted from the disc.
%%%
%%% @end
%%% ----------------------------------------------------------

-spec delete_log(Name::string()) -> ok.

delete_log(Name) when is_atom(Name) ->
    delete_log(atom_to_list(Name));
delete_log(Name) ->
    logRamServer:delete_log(Name).

%%% ----------------------------------------------------------
%%% @doc
%%% Write a message to a RAM log.
%%%
%%% === Arguments ===
%%%
%%% Name
%%% ```
%%% Name of the RAM log.
%%% '''
%%%
%%% Tag : {Module, Line, Pid}
%%% ```
%%% Should be specified as {?MODULE, ?LINE, self()}.
%%% Used for black list handling, see below.
%%% '''
%%% Data : DataElement | [DataElement]
%%%
%%% DataElement : {Severity, Msg} | {Severity, Msg, BlackList}
%%%
%%% Severity   
%%% ```
%%% Used for filtering of the log entries
%%% All write attempts that are tagged with higher severity level than the filter level will be disgarded.
%%% The default filter level is 1 (one). The filter level is set by a coli command: /log/ramlog..
%%% 'error' and 'warning' cannot be filtered.
%%% However, these 2 levels can use black list option to prevent flooding the log with similar entries, see below.
%%% '''
%%% BlackList
%%% ```
%%% Used to prevent similar entries to fill the logs. When set to:
%%%  - 'false', all entries are written to the log.
%%%  - 'file' only one entry with a specific Tag will be written to the log.
%%%  -  an integer, only one entry with the same tag will be witten to the file during the specified number of seconds.
%%% '''
%%% Msg : IoString | {IoString, IoArgs}
%%% ```
%%% The log message.
%%% Msg is formatted using io_lib:format(IoString, []) or io_lib:format(IoString, IoArgs)
%%% '''
%%% IoString
%%% ```
%%% String, as specified in io:format
%%% '''
%%%
%%% IoArgs
%%% ```
%%% Arguments, as specified in io:format
%%% '''
%%%
%%% 
%%% === Macros ===
%%% Below are examples of macros which could be defined in a user application
%%% ```
%%% -define(LOG_RAM(__Sev, __Msg),
%%% 	logRamI:write_log("MyLogName",
%%% 			  {?MODULE, ?LINE, self()}, 
%%% 			  {__Sev, __Msg})).
%%% 
%%% -define(LOG_RAM(__Sev, __Msg, __BL),
%%% 	logRamI:write_log("MyLogName",
%%% 			  {?MODULE, ?LINE, self()}, 
%%% 			  {__Sev, __Msg, __BL})).
%%%
%%% -define(LOG_RAM(__MultiMsg),
%%% 	logRamI:write_log("MyLogName",
%%% 			  {?MODULE, ?LINE, self()}, 
%%% 			  __MultiMsg)).
%%% '''
%%% 
%%% === Examples ===
%%% Examples using the above macros.
%%% 
%%% ```
%%%    ?LOG_RAM(1, {"Some text ~p~n", [Arg]}),
%%% '''
%%% An example of an error severity log entry.<br/>
%%%  If the same error 
%%% occures again it will be ignored if the entry would be written
%%% to the same file.
%%% 
%%% ```
%%%    ?LOG_RAM(error, {"Some serious error ~p~n", [Reason]}, file),
%%% '''
%%% An example specifying different messages for different severity levels.<br/>
%%% If the severity filtering is 1 or 2 the first message is written to the log.<br/>
%%% If the severity filtering is 3 or greater the second message is written to the log
%%% and it is put in the black list.<br/>
%%% for 20 seconds; i.e. write attempts with the same tag  
%%% will be ignored for 20 seconds.
%%% ```
%%%    ?LOG_RAM([{1, {"Severity level ~p~n", [1]}},
%%%              {3, {"Severity level ~p and put in black list~n", [3]}, 20}]).
%%% '''
%%% @end
%%% ----------------------------------------------------------

-spec write_log(Name, Tag, Data) -> ok | {error, Reason} when
    Name :: string(),
    Tag :: {Module :: atom(), Line :: integer(), Pid :: pid()},
    Data :: DataElements | [DataElements],
    DataElements :: {Severity, Msg} | {Severity, Msg, BlackList} |
		    {Severity, Fun} | {Severity, Fun, BlackList},
    Severity :: error | warning | 1..9,
    Msg :: string() | {string(), Args},
    Args :: [term()],
    BlackList ::  false | file | Seconds,
    Seconds :: integer(),
    Fun :: fun(),
    Reason :: term().

write_log(Name, Tag, Data) when is_atom(Name) ->
    write_log(atom_to_list(Name), Tag, Data);
write_log(Name, Tag, Data) ->
    case verify_write(Name, Tag, Data) of
	ok -> 
	    Time   = os:timestamp(),
	    Format = format_data(Data),
	    logRamServer:write_log(Name, Tag, Time, lists:sort(Format));
	Error ->
	    Error
    end.
	    

%%% ----------------------------------------------------------
%%% @doc
%%% Callback function invoked when ESI is requested.
%%%
%%% Writes the current RAM content to disc.
%%% @end
%%% ----------------------------------------------------------
%%% @private
generate_esi() ->
    logRamServer:generate_esi().

%%% ----------------------------------------------------------
%%% @doc
%%% Only for testing.
%%% Writes the current RAM into a file
%%% @end
%%% ----------------------------------------------------------
%%% @private
write_to_file() ->
    logRamServer:generate_esi().

write_to_file(Log) ->
    logRamServer:generate_esi(Log).

%%% ----------------------------------------------------------
%%% @doc
%%% Callback function invoked from COLI
%%%
%%% Set the severity filter level.
%%% @end
%%% ----------------------------------------------------------
%%% @private
coli_set_severity([Log, Severity]) ->
    case css(Log, to_int(Severity)) of
	ok ->
	    io:format("ok~n");
	{error, {log_not_found, _}} -> 
	    io:format("log not found~n");
	{error, illegal_severity}   -> 
	    io:format("severity level must be between 1 and 9~n")
    end;
coli_set_severity(_) ->
    io:format("Arguments: LogName Severity. ~n"
	      "Severity is an integer between 1 and 9.~n"),
    ok.

to_int(Int) ->
    try
	list_to_integer(Int)
    catch
	error:_ ->
	    {error, illegal_severity}
    end.

css(Log, Severity)
  when is_integer(Severity) andalso
       Severity > 0 andalso
       Severity < 10 ->
    logRamServer:set_severity(Log, Severity);
css(_Log, _Severity) ->
    {error, illegal_severity}.

%%% ----------------------------------------------------------
%%% @doc
%%% Only for testing
%%% Set the filtering level
%%% @end
%%% ----------------------------------------------------------
%%% @private
set_severity(Log, Severity)
  when is_integer(Severity) andalso
        Severity > 0 andalso
	Severity < 10 ->
    logRamServer:set_severity(Log, Severity);
set_severity(_Log, Severity) ->
    sysInitI:info_msg("~p:set_severity(~p) ~n"
		      "Severity must be between 1 and 9 ~n",
		      [?MODULE, Severity]),
    {error, illegal_severity}.

%%% ----------------------------------------------------------
%%% @doc
%%% Only for testing
%%% set options
%%% @end
%%% ----------------------------------------------------------
%%% @private
set_options(Log, Options)
  when is_list(Options) ->
    logRamServer:set_options(Log, so_check(Options)).



so_check(Options) ->
    Local     = proplists:get_value(local,     Options),
    Encrypted = proplists:get_value(encrypted, Options),
    so_check_rc(Local, Encrypted),
    Options2  = proplists:delete(local,     Options),
    proplists:delete(encrypted, Options2).
    
so_check_rc(undefined, undefined) ->
    ok;
so_check_rc(undefined, _) ->
    sysInitI:warning_msg("Not allowed to update option 'encrypted' ~n");
so_check_rc(_, undefined) ->
    sysInitI:warning_msg("Not allowed to update option 'local' ~n");
so_check_rc(_, _) ->
    sysInitI:warning_msg("Not allowed to update options 'local' and 'encrypted' ~n").

%%% ----------------------------------------------------------
%%% @doc
%%% Only for testing.
%%% Print loop data
%%% @end
%%% ----------------------------------------------------------
%%% @private
print_loop_data() ->
    logRamServer:print_loop_data().

%%% @private
print_loop_data(Log) ->
    logRamServer:print_loop_data(Log).

%%% ----------------------------------------------------------
%%% @doc
%%% Only for testing.
%%% Print RAM log contents
%%% @end
%%% ----------------------------------------------------------
%%% @private
print_logs() ->
    logRamServer:print_logs().

%%% @private
print_logs(Log) ->
    logRamServer:print_logs(Log).


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%=======================================================
%% format data to list, and add black list if missing
%%=======================================================
format_data({Sev, Msg}) ->
    format_data([{Sev, Msg, false}]);
format_data({Sev, Msg, BL}) ->
    format_data([{Sev, Msg, BL}]);
format_data(Data) ->
    fd(Data, []).

fd([], Acc) ->
    lists:reverse(Acc);
fd([{Sev, Msg} | T], Acc) ->
    fd(T, [{Sev, Msg, false} | Acc]);
fd([{Sev, Msg, BL} | T], Acc) ->
    fd(T, [{Sev, Msg, BL} | Acc]).



%%=======================================================
%% verify write_log parameters -> ok | {error, Reason}
%%=======================================================
verify_write(Name, Tag, Data) ->
    try 
	vw_name(Name),
	vw_tag(Tag),
	vw_data(format_data(Data)),
	ok
    catch
	throw:{?MODULE, Error} ->
	    {error, Error}
    end.

vw_name(Name) when is_list(Name) ->
    ok;
vw_name(Name) ->
    throw({?MODULE, {invalid_name, Name}}).

vw_tag({Mod, Line, Pid}) when is_atom(Mod) andalso
			      is_integer(Line) andalso
			      is_pid(Pid) ->
    ok;
vw_tag(Tag) ->
    throw({?MODULE, {invalid_tag, Tag}}).


vw_data([]) ->
    ok;
vw_data([{Severity, Msg, BlackList} | T]) ->
    vw_severity(Severity),
    vw_msg(Msg),
    vw_black_list(BlackList),
    vw_data(T).

vw_severity(error) ->
    ok;
vw_severity(warning) ->
    ok;
vw_severity(Severity) when is_integer(Severity) andalso
			   Severity > 0 andalso
                           Severity < 10 ->
    ok;
vw_severity(Severity) ->
    throw({?MODULE, {invalid_severity, Severity}}).

vw_msg(Msg) when is_list(Msg) ->
    ok;
vw_msg({Msg, Args}) when is_list(Msg) andalso is_list(Args) ->
    ok;
vw_msg(Msg) when is_function(Msg) ->
    ok;
vw_msg(Msg) ->
    throw({?MODULE, {invalid_msg, Msg}}).

vw_black_list(false) ->
    ok;
vw_black_list(file) ->
    ok;
vw_black_list(Time) when is_integer(Time) ->
    ok;
vw_black_list(BL) ->
    throw({?MODULE, {invalid_black_list, BL}}).




%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

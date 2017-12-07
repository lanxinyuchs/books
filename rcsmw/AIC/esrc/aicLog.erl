%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	aicLog.erl %
%%% @author efilgri
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R10A/3
%%%
%%% @doc ==AIC log module=
%%% This module handles AIC log
-module(aicLog).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R10A/3').
-date('2017-07-18').
-author('efilgri').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
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
%%% R2A/1      2014-03-21 etxtory     Created
%%% R3A/2      2015-03-02 etxtory     Copy to wrong log-fn
%%% R3A/3      2015-03-11 etxderb     Handle null strings in log line
%%% R3A/4      2015-03-16 etxderb     General string fault handling
%%% R4A/1      2015-06-15 etxpejn     Support for Dual DU, FruId added to AiLog
%%% R4A/3      2015-09-25 etxpejn     HU20650: Moved rpc:call to logI:write_log
%%% R5A/1      2016-03-07 evanbel     Adding link to the AI log in www doc root
%%% R5A/2      2016-04-06 evanbel     Adding support for AI GUI progress bar
%%% R6A/2      2016-06-07 evanbel     Added functions for fetching logs
%%% ----------------------------------------------------------
%%% R9A/1      2017-01-30 etxderb     Not revert log at load backup+aicServer
%%% R9A/2      2017-03-06 evanbel     Make nl version file for use in help pages
%%% ----------------------------------------------------------
%%% R10A/1     2017-05-12 evanbel     HV86583 add percentage to progress bar status
%%% R10A/2     2017-07-11 evanbel     Changed file to write NL version to
%%% R10A/3     2017-07-17 efilgri     HW12776 - changed method of reading file & backup options
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([open_log/0]).  %% Called from aicServer:activate().
-export([get_nl_logs/0]).
-export([get_internal_logs/0]).
-export([write_error/2]).
-export([write_progress_bar/1]).
-export([write_log/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include("RmeAI.hrl").

-define(NL_LOGFILE, "nl_log").
-define(NL_DEBUGFILE, "nl_debug").
-define(ERRORLOG, "error_log").
-define(NL_LOGDIR, "/opt/rcs_ee/mounts/rcspartition/rcs/bootlogs").
-define(NL_SYSLOG, "/opt/rcs_ee/mounts/os_logs/log/syslog").
-define(NL_VSN_FILE, "nl_product_version.txt").
-define(NL_VSN_FROM_LOG, "nl_version_ai.txt").
-define(NL_VSN_FROM_NL, "nl_prod_ver_export.txt").

%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% Opens NL log (nl_log.1)
%%% ----------------------------------------------------------
open_log() ->
    LogDir = filename:join([sysEnv:rcs_dir(), "networkloader"]),
    case file:read_file_info(LogDir) of
        {error, enoent} ->
            file:make_dir(LogDir);
        _ ->
            ok
    end,
    
     LogFn = filename:join(LogDir, ?NL_LOGFILE),
    case file:read_file_info(LogFn ++ ".1") of
	{ok, _} ->
	    %% NL LogFile exist
	    %% Open the LogFile for writing
	    do_open_log(LogFn);
	_ ->
	    %% No NL LogFile exist
	    consume_nl_info(LogFn, LogDir),
	    do_open_log(LogFn),
	    convert_nl_log()
    end,
    create_vsn_file().


consume_nl_info(LogFn, LogDir) ->
    %% Copy the NL generated nl_log from bootlogs so log-entries
    %% from NL is kept in the case going back to NL due to
    %% Netconf (siteEquipment/siteBasic) loading error.
    BNL = filename:join(sysEnv:rcs_dir(), "bootlogs/nl_log.1"),
    LogFn = filename:join(LogDir, ?NL_LOGFILE),
    file:copy(BNL, LogFn ++ ".1"),

    %% Make a link to the NL log in www root so it can be displayed
    %% in the post-integration AI GUI.
    WWWDocRoot = sysEnv:www_doc_root(),
    os:cmd("ln -s " ++ LogFn ++ ".1 "
           ++ filename:join(WWWDocRoot, "ailog.txt" )),

    %% Make a link to the NL version file to be displayed in the
    %% GUI help dialog
    os:cmd("ln -s " ++ filename:join([?NL_LOGDIR, ?NL_VSN_FILE]) ++ " "
	  ++ filename:join(WWWDocRoot, "nl_version.txt")).


do_open_log(LogFn) ->
    %% Same params as NL/nl_log.erl for disk_log:open.
    MaxSize = 50000,
    MaxNoFiles = 1,
    disk_log:open([{name, ?NL_LOGFILE},
                   {file, LogFn},
                   {format, external},
                   {mode, read_write},
                   {type, wrap},
                   {size, {MaxSize, MaxNoFiles}}]).

convert_nl_log() ->
    %% Check if there exist a network loader log (nl_log).
    %% If so, convert the nl_log into SAF-log format.
    %% nl_log format:
    %% "2014/01/27 15:31:27 Autointegration waiting for user input\n"

    BNL = filename:join(sysEnv:rcs_dir(), "bootlogs/nl_log.1"),
    case file:read_file(BNL) of
        {ok, Bin} ->
            Bins = binary:split(Bin, <<"\n">>, [global]),
            case catch convert_nl_log(Bins) of
                ok ->
                    ok;
                Error ->
                    error_msg("~p:convert_nl_log()~n"
                              "~p~n",
                              [?MODULE, Error])
            end;
        {error, _} ->
            ok
    end.

%%% ----------------------------------------------------------
%%% write_progress_bar(atom()) ->
%%%        ok.
%%%
%%% Writes the progressbar.txt file in docroot for the AI GUI
%%% progress bar.
%%% ----------------------------------------------------------
write_progress_bar(?ConfigLevel_SITE_CONFIG_COMPLETE) ->
    do_write_progress_bar({"70% Site Config Complete", 70});
write_progress_bar(?ConfigLevel_OSS_ACTIVATING_CONFIGURATION) ->
    do_write_progress_bar({"75% OSS Activating Configuration", 75});
write_progress_bar(?ConfigLevel_ACTIVATING_FEATURES) ->
    do_write_progress_bar({"80% Activating Features", 80});
write_progress_bar(?ConfigLevel_OSS_CONTROL_NODE_CONN) ->
    do_write_progress_bar({"85% OSS Node Conn", 85});
write_progress_bar(?ConfigLevel_UNLOCKING_CELLS) ->
    do_write_progress_bar({"90% Unlocking Cells", 90});
write_progress_bar(?ConfigLevel_INTEGRATION_COMPLETE) ->
    do_write_progress_bar({"95% Integration Complete", 95});
write_progress_bar(?ConfigLevel_READY_FOR_SERVICE) ->
    do_write_progress_bar({"100% Ready for Service", 100});
write_progress_bar(_) ->
    ok.

do_write_progress_bar({First, Second}) when is_integer(Second)->
    do_write_progress_bar(io_lib:format("~s|~s", [First, integer_to_list(Second)]));

do_write_progress_bar(Status) when is_list(Status)->
    WWWDir = sysEnv:www_doc_root(),
    PFile = filename:join([WWWDir, "progressbar.txt"]),
    case file:write_file(PFile, list_to_binary(Status)) of
        ok ->
            ok;
        {error, _Reason}->
            write_log(info, "Write_progress_bar failed to write")
    end.

%%% ----------------------------------------------------------
%%% Writes netconf errors to file and to log files.
%%% ----------------------------------------------------------
write_error(Type, Msg) ->
    LogDir = filename:join([sysEnv:rcs_dir(), "networkloader"]),
    LogFn = filename:join(LogDir, ?ERRORLOG),
    case file:open(LogFn, [raw, write, append]) of
        {ok, Fd} ->
            file:write(Fd, list_to_binary(Msg)),
            file:close(Fd);
        _ ->
            ok
    end,
    write_log(Type, Msg).

%%% ----------------------------------------------------------
%%% Logging in SAF-log and nl_log.1
%%% Type: see logI (info | error)
%%% Msg does not contain ~n; needs to be added for NL log.
%%% ----------------------------------------------------------
write_log(Type, Msg) ->
    do_log(Msg ++ "~n", [], ?NL_LOGFILE),
    logI:write_log("AiLog", "", Type, Msg).

%% LogEntry: 2013/10/17 03:00:48 AUTOINTEGRATION MANAGEMENT STARTED
%% LogEntry: Date       UTC      Format/Data
do_log(Format, Data, LogName) ->
    TS = get_time_stamp(),
    Str = lists:flatten(TS ++ io_lib:format(Format, Data)),
    disk_log:blog(LogName, list_to_binary(Str)).

%%% ----------------------------------------------------------
%%% Return a list of logs names needed when exporting the AI log.
%%% ----------------------------------------------------------
get_nl_logs()->
    Logfile = filename:join(?NL_LOGDIR, ?NL_LOGFILE ++ ".1"),
    %%Dbgfile = filename:join(?NL_LOGDIR, ?NL_DEBUGFILE ++ ".1"),
    case file:read_file(Logfile) of
        {ok, LB}->
	    {ok, LB};
	{error, Reason} ->
	    {error, Reason}
    end.

get_internal_logs()->
    Tempfile = "/tmp/internal.tar.gz",
    file:delete(Tempfile),
    Logfiles = filename:join([?NL_LOGDIR, ?NL_LOGFILE ++ ".1"]) ++ " " ++ 
               filename:join([?NL_LOGDIR, ?NL_DEBUGFILE ++ ".1"]) ++ " " ++ 
               filename:join([?NL_LOGDIR, "netloader.log"]) ++ " " ++ 
               filename:join([?NL_LOGDIR, "erlang.log.1"]) ++ " " ++ 
               ?NL_SYSLOG,
    os:cmd("tar cvfz " ++ Tempfile ++ " " ++ Logfiles),
    case file:read_file(Tempfile) of
        {ok, Bin} ->
            Crypto = encrypt(Bin),
            {ok, Crypto};
        {error, Reason} ->
            {error, Reason}
    end.

encrypt(Bin) ->
    crypto:block_encrypt(aes_cfb128,
                         <<"1234567890ABCDEF">>,
                         <<"1234567890ABCDEF">>,
                         Bin).

%% TimeStamp = "2013/10/17 03:00:48 "
%% TimeStamp = "2014/01/01 01:01:01 "
get_time_stamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
    YearF = integer_to_list(Year),
    MonthF = double_digit(Month),
    DayF = double_digit(Day),
    HourF = double_digit(Hour),
    MinF = double_digit(Min),
    SecF = double_digit(Sec),
    YearF ++ "/" ++ MonthF ++ "/" ++ DayF ++ " " ++
        HourF ++ ":" ++ MinF ++ ":" ++ SecF ++ " ".

double_digit(Integer)
  when Integer =< 9 ->
    "0" ++ integer_to_list(Integer);
double_digit(Integer) ->
    integer_to_list(Integer).

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% Converting NL-log to SAF-format
%%% ----------------------------------------------------------
convert_nl_log([<<>>]) ->
    ok;
convert_nl_log([Bin | T]) ->
    case do_convert(Bin) of
        {ok, {TS, Text}} ->
            logI:write_log("AiLog", "", info, TS, Text);
        {error, _Reason} ->
            ok
    end,
    convert_nl_log(T).


do_convert(Bin) ->
    try
        Str = binary_to_list(Bin),
        [Date, Time | _] = string:tokens(Str, " "),
        Len = string:len(Date) + string:len(Time) + 3,
        Text = string:substr(Str, Len),
        TS = datetime_to_now(Date, Time),
        {ok, {TS, Text}}
    catch
        T:E ->
            S = "convert_nl_log/1 line parse error: ~p, ignoring~n",
            warning_msg(S, [{T, E}]),
            {error, parse_error}
    end.



 %% Date = "2014/01/27"
 %% Time = "15:31:27"
 datetime_to_now(Date, Time) ->
     GregSecs1970 = 62167219200, %% Gregorian seconds 1970/01/01 0:0:0
     [Year, Month, Day] = string:tokens(Date, "/"),
     [Hour, Min, Sec] = string:tokens(Time, ":"),
     DateTime = {{list_to_integer(Year), list_to_integer(Month), list_to_integer(Day)},
                 {list_to_integer(Hour), list_to_integer(Min), list_to_integer(Sec)}},

     GregSecs1970 = 62167219200, %% Gregorian seconds 1970/01/01 0:0:0
     GSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
     ESeconds = GSeconds - GregSecs1970,
     {ESeconds div 1000000, ESeconds rem 1000000, 0}.

%%% ----------------------------------------------------------
%%% Create version file
%%% ----------------------------------------------------------
create_vsn_file() ->
    Filename = filename:join(sysEnv:www_doc_root(), ?NL_VSN_FROM_LOG),
    Exported_ver = filename:join(?NL_LOGDIR, ?NL_VSN_FROM_NL),
    case file:read_file(Exported_ver) of
    {ok, Bin} ->
          file:write_file(Filename, Bin);
    {error, _} ->
          Cmd = string:concat("sed -n -e 's/^.*Running version: //p' ",
          filename:join(?NL_LOGDIR, "nl_log.1")),
          case os:cmd(Cmd) of
          [] ->
             %% last ditch effort
             EEverfile = filename:join(?NL_LOGDIR, ?NL_VSN_FILE),
             case file:read_file(EEverfile) of
             {ok, EEBin} ->
                file:write_file(Filename,EEBin);
             {error,_} ->
                Fake_version = "CNX9012629-R7A77",
                file:write_file(Filename, Fake_version)
             end;
          Result ->
            [LastVsn | _] = lists:reverse(string:tokens(Result, "\n")),
            file:write_file(Filename, LastVsn)
          end
    end.

%%% ----------------------------------------------------------
%%% Error-msg
%%% ----------------------------------------------------------
error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

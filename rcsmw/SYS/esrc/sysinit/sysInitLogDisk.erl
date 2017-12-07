%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysInitLogDisk.erl %
%%% @author etomist
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R4A/R5A/R10A/R11A/1

%%% @doc ==Header==
%%% Writes erlang crashes, error reports, error msg
%%% and important erlang system events.
%%% @end

-module(sysInitLogDisk).
-vsn('/main/R4A/R5A/R10A/R11A/1').
-date('2017-09-05').
-author('etomist').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
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
%%% Rev        Date    Name      What
%%% -----      ------  --------  ------------------------
%%% R4A/1      2015-09-07 etxtory   First version
%%% R5A/1      2016-02-02 etxarnu   Corrected erlang_disk dir for sim
%%% R5A/2      2016-03-18 etxarnu   Changed size erlang_disk 
%%% R11A/1     2017-09-05 etomist   HV82742
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([write_event/1,
	 write_event/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-define(LOG_NAME, "erlang_system_log").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Writes an event to persistent log.
%%% Only very important events should use be logged.
%%% @end
%%% ----------------------------------------------------------
write_event(String, Args) ->
        write_event(io_lib:format(String, Args)).

write_event(Str) ->
    case disk_log:blog(?LOG_NAME, list_to_binary(Str)) of
    ok ->
        ok;
    {error, no_such_log} ->
        create_log(),
        disk_log:blog(?LOG_NAME, list_to_binary(Str)),
        ok;
    {error, Reason} ->
        %% Use io:format so we don't get a ERROR LOGGER loop
        io:format("~p: Failed writing to ~p: ~p~n",
                  [?MODULE, ?LOG_NAME, Reason]),
        ok
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
create_log() ->
    LogDir = filename:join([sysEnv:rcs_dir(), "erlang_disk"]),
    filelib:ensure_dir(LogDir),
    LogFn = filename:join([LogDir, ?LOG_NAME]),
    MaxSize = 200000,
    MaxNoFiles = 5,
    disk_log:open([{name, ?LOG_NAME},
                   {file, LogFn},
                   {format, external},
                   {mode, read_write},
                   {type, wrap},
                   {size, {MaxSize, MaxNoFiles}}]).

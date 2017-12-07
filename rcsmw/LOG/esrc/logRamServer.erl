%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	logRamServer.erl %
%%%
%%% Description:
%%%
%%% Server for the RAM log service.
%%%
%%% ----------------------------------------------------------
-module(logRamServer).
-behaviour(gen_server).
-vsn('/main/R5A/R6A/R7A/R8A/R9A/1').
-date('2017-03-28').
-author('uabesvi').
-shaid('31926b2842acde35ca6ced9bb555a0a34bc2eeb4').
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
%%% -----      -------    --------    ------------------------
%%% R5A/1-7    2016-02-10 uabesvi     Created
%%% R9A/1      2017-03-27 uabesvi  Added code for compress
%%% ----------------------------------------------------------
%%%
%%% #---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/0]).
-export([stop/0]).


-export([create_log/2]).
-export([delete_log/1]).

-export([write_log/4]).

-export([generate_esi/0]).


%% Debug functions
-export([generate_esi/1]).
-export([generate_esi_all_logs/0]).

-export([set_severity/2]).
-export([set_options/2]).

-export([get_loop_data/0]).
-export([print_loop_data/0]).
-export([print_loop_data/1]).
-export([print_logs/0]).
-export([print_logs/1]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 code_change/3, 
	 terminate/2]).


-include("log.hrl").

-define(DEF_SIZE,      1000).
-define(DEF_MAX_FILES, 3).


%% these values are valid if the option zip is set
-define(MAX_ZIP_TOTAL_SIZE,  15000).
-define(MAX_ZIP_FILE_SIZE,   1000).
-define(MAX_ZIP_NOOF_FILES,  30).
%% these values are valid if the option zip is not set
-define(MAX_TOTAL_SIZE,  6000).
-define(MAX_FILE_SIZE,   1000).
-define(MAX_NOOF_FILES,  10).

-define(CALL_TO,  30000).

-record(state, {logs = #{}}). %% {Name, {Options, Pid, Ref}}



%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% start() -> ok.
%% 
%%========================================================================
start() ->
    ServerName = {local, ?MODULE},
    Module     = ?MODULE,
    Args       = [],
    Options    = [],
    gen_server:start_link(ServerName, Module, Args, Options).


%%========================================================================
%% stop() -> ok.
%% 
%%========================================================================
stop() ->
    try
	gen_server:call(?MODULE, stop)
    catch exit:E ->
	    {error, E}
    end.


%%========================================================================
%% create_log(Name, Options) -> ok | {error, Reason}
%% 
%% @doc
%% For options refer to logRamI.erl
%% @end
%%========================================================================
create_log(Name, Options) ->
    cl(validate_options(Options), Name).

cl({ok, Res}, Name) ->
    Data = {create_log, Name, Res},
    %% sysInitI:info_msg("~p: Opening log ~p~n", [?MODULE , Name]),
    try
	gen_server:call(?MODULE, Data, 60000)
    catch exit:E ->
	    {error, E}
    end;
cl({error, Error}, _) ->
    sysInitI:warning_msg("~p create_log with invalid options ~p~n", 
			 [?MODULE, Error]),
    {error, Error}.


%%========================================================================
%% delete_log(Name) -> ok.
%% 
%% 
%%========================================================================
delete_log(Name) ->
    sysInitI:info_msg("~p: Deleting log ~p~n", [?MODULE, Name]),
    try
	gen_server:call(?MODULE, {delete_log, Name})
    catch exit:E ->
	    {error, E}
    end.

%%========================================================================
%% write_log(Name, Tag, Time, Data) -> ok | {error, Reason}
%% 
%% @doc
%% For the attributes refer to logRamI.erl
%% @end
%%========================================================================
write_log(Name, Tag, Time, Data) when is_tuple(Data) ->
    write_log(Name, Tag, Time, [Data]);
write_log(Name, Tag, Time, Data) ->
    case check_write_level(Name, Data) of
	ok  -> 
	    try
		gen_server:call(?MODULE, {write_log, Name, Tag, Time, Data})
	    catch exit:E ->
		    {error, E}
	    end;
	{error, level} ->
	    ok;
	Error ->
	    Error
    end.

%%========================================================================
%% generate_esi() -> ok.
%% 
%% @doc
%% Callback function when ESI is generated.
%% @end
%%========================================================================
generate_esi() ->
    try
	gen_server:call(?MODULE, generate_esi, ?CALL_TO)
    catch exit:E ->
	    {error, E}
    end.

%%========================================================================
%% set_severity(Name, Level) -> ok.
%% 
%% Set the filtering level for a log
%%========================================================================
%%% @private
set_severity(Name, Level) ->
    try
	gen_server:call(?MODULE, {set_severity, Name, Level}, ?CALL_TO)
    catch exit:E ->
	    {error, E}
    end.

%%========================================================================
%% set_options(Name, Options) -> ok.
%% 
%% Set options for a log
%%========================================================================
%%% @private
set_options(Name, Options) ->
    try
	gen_server:call(?MODULE, {set_options, Name, Options}, ?CALL_TO)
    catch exit:E ->
	    {error, E}
    end.

%%========================================================================
%% get_loop_data() -> ok.
%% 
%% Debug functions
%%========================================================================
%%% @private
generate_esi(Log) ->
    try
	gen_server:call(?MODULE, {generate_esi, Log}, ?CALL_TO)
    catch exit:E ->
	    {error, E}
    end.

%%% @private
generate_esi_all_logs() ->
    try
	gen_server:call(?MODULE, generate_esi_all_logs, ?CALL_TO)
    catch exit:E ->
	    {error, E}
    end.

%%% @private
get_loop_data() ->
    try
	gen_server:call(?MODULE, get_loop_data, ?CALL_TO)
    catch exit:E ->
	    {error, E}
    end.

%%% @private
print_loop_data() ->
    try
	gen_server:call(?MODULE, print_loop_data, ?CALL_TO)
    catch exit:E ->
	    {error, E}
    end.

%%% @private
print_loop_data(Log) ->
    try
	gen_server:call(?MODULE, {print_loop_data, Log}, ?CALL_TO)
    catch exit:E ->
	    {error, E}
    end.

%%% @private
print_logs() ->
    try
	gen_server:call(?MODULE, print_logs, ?CALL_TO)
    catch exit:E ->
	    {error, E}
    end.

%%% @private
print_logs(Log) ->
    try
	gen_server:call(?MODULE, {print_logs, Log}, ?CALL_TO)
    catch exit:E ->
	    {error, E}
    end.



%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% init(_) -> {ok, State}
%%========================================================================
init(_) ->
    process_flag(trap_exit, true), %% to receive terminate at shutdown
    logI:register_esi_cb(?MODULE),
    {ok, #state{}}.



%%========================================================================
%% handle_call(Msg, From, State) -> {reply, Res, State}
%%========================================================================
handle_call({create_log, Name, Options}, _, #state{logs = Logs} = State) ->
    OldLogData = maps:get(Name, Logs, undefined),
    NewOptions = check_opts(Options, Name),
    {Res, NewState} = handle_create_log(OldLogData, Name, NewOptions, State),
    {reply, Res, NewState};

handle_call({delete_log, Name}, _From, #state{logs = Logs} = State) ->
    Log = maps:get(Name, Logs, undefined),
    {Res, NewState} = handle_delete_log(Log, Name, State),
    {reply, Res, NewState};

handle_call({write_log, Name, Tag, Time, Data},
	     _From,
	    #state{logs = Logs} = State) ->
    Log = maps:get(Name, Logs, undefined),
    Res = handle_write_log(Log, Name, Tag, Time, Data),
    {reply, Res, State};
	    

handle_call(generate_esi, 
	    _From,
	    #state{logs = Logs} = State) ->
    Res = handle_generate_esi(Logs),
    {reply, Res, State};

handle_call({generate_esi, Log}, 
	    _From,
	    #state{logs = Logs} = State) ->
    Res = handle_generate_esi(Log, maps:get(Log, Logs, undefined)),
    {reply, Res, State};

handle_call(generate_esi_all_logs, 
	    _From,
	    #state{logs = Logs} = State) ->
    Res = handle_generate_esi(Logs),
    {reply, Res, State};
handle_call({set_severity, Name, Level},
	    _From,
	    #state{logs = Logs} = State) ->
    Log = maps:get(Name, Logs, undefined),
    case handle_set_severity(Log, Name, Level) of
	{ok, NewOptions} ->
	    {reply, ok, State#state{logs = Logs#{Name => NewOptions}}};
	Error ->
	    {reply, Error, State}
    end;

handle_call({set_options, Name, Options},
	    _From,
	    #state{logs = Logs} = State) ->
    Log = maps:get(Name, Logs, undefined),
    case handle_set_options(Log, Name, Options) of
	{ok, Res, NewOptions} ->
	    {reply, Res, State#state{logs = Logs#{Name => NewOptions}}};
	Error ->
	    {reply, Error, State}
    end;
	    

handle_call(get_loop_data, _From, State) ->
    {reply, format_state(State), State};

handle_call(print_loop_data, _From, #state{logs = Logs} = State) ->
    Res = handle_print_loop_data(Logs),
    {reply, Res, State};

handle_call({print_loop_data, Log}, _From, #state{logs = Logs} = State) ->
    Data = maps:get(Log, Logs, undefined),
    Res  = handle_print_loop_data_log(Log, Data),
    {reply, Res, State};

handle_call(print_logs, _From, #state{logs = Logs} = State) ->
    Res = handle_print_logs(Logs),
    {reply, Res, State};

handle_call({print_logs, Log}, _From, #state{logs = Logs} = State) ->
    Data = maps:get(Log, Logs, undefined),
    Res = handle_print_log(Log, Data),
    {reply, Res, State};

handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


%%========================================================================
%% handle_cast(Msg, State) -> {noreply, State}
%%========================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.

%%========================================================================
%% handle_info(Info, State) -> {noreply, State}
%%========================================================================
handle_info({'DOWN', _MRef, process, Pid, Reason}, State) ->
    sysInitI:info_msg("~p: DOWN From ~p Reason ~p~n", [?MODULE, Pid, Reason]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


%%========================================================================
%% code_change(OldVsn, State, Extra) -> {ok, State}
%%========================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%========================================================================
%% terminate(Reason, State) -> Reason
%%========================================================================
terminate(Reason, #state{logs = Logs}) ->
    maps:fold(fun terminate_log/3, [], Logs),
    Reason.

terminate_log(_, {_, Pid, Ref}, Acc) ->
    erlang:demonitor(Ref, [flush, info]),
    logRamLog:stop(Pid, terminated),
    Acc.





%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% handle_create_log
%%========================================================================
handle_create_log(undefined, Name, Options, #state{logs = Logs} = State) ->
    IsLocal     = maps:get(local,     Options, false),
    IsEncrypted = maps:get(encrypted, Options, false),
    Zip         = maps:get(zip,       Options, false),
    logLib:log_storage_mode_create(Name, IsLocal, IsEncrypted, Zip),
    sysInitI:info_msg("~p: Opening log ~p~n", [?MODULE , Name]),
    {ok, Pid}   = logRamLog:start(Name, Options),
    MonRef      = erlang:monitor(process, Pid),
    {ok, State#state{logs = Logs#{Name => {Options, Pid, MonRef}}}};
handle_create_log({_, Pid, _}, Name, Options, State) ->
    ok = logRamLog:set_options(Pid, Name, Options),
    {ok, State}.


%%========================================================================
%% handle_delete_log
%%========================================================================
handle_delete_log({_, Pid, _}, Name, #state{logs = Logs} = State) ->
    logRamLog:stop(Pid, deleted),
    logLib:log_storage_mode_delete(Name),
    {ok, State#state{logs = maps:remove(Name, Logs)}};
handle_delete_log(undefined, _Name, State) ->
    {ok, State}.


%%========================================================================
%% handle_write_log
%%========================================================================
handle_write_log({_, Pid, _}, Name, Tag, Time, Data) ->
    logRamLog:write_log(Pid, Name, Tag, Time, Data);
handle_write_log(undefined, Name, _, _, _Data) ->
    {error, {log_not_found, Name}}.




%%========================================================================
%% handle_generate_esi
%%========================================================================
handle_generate_esi(Logs) ->
    maps:fold(fun hfw/3, [], Logs).

hfw(Name, {_, Pid, _}, Acc) ->
    logRamLog:generate_esi(Pid, Name),
    Acc.

handle_generate_esi(Name, undefined) ->
    sysInitI:info_msg("Log ~p not found. Could not write to file~n", [Name]);
handle_generate_esi(Name, {_, Pid, _}) ->
    logRamLog:generate_esi(Pid, Name).


%%========================================================================
%% handle_set_severity
%%========================================================================
handle_set_severity({OldOptions, Pid, Ref}, Name, LogLevel) ->
    logRamLog:set_severity(Pid, Name, LogLevel),
    {ok, {OldOptions#{severity => LogLevel}, Pid, Ref}};
handle_set_severity(undefined, Name, _) ->
    {error, {log_not_found, Name}}.

%%========================================================================
%% handle_set_options
%%========================================================================
handle_set_options({OldOptions, Pid, Ref}, Name, NewOptions) ->
    OldSize = maps:get(size,       OldOptions),
    OldMaxF = maps:get(maxNoFiles, OldOptions),
    OldHead = maps:get(header,     OldOptions),
    OldZip  = maps:get(zip,        OldOptions),
    OldSeq  = maps:get(sequence,   OldOptions),
    OldLine = maps:get(line,       OldOptions),
    OldSev  = maps:get(severity,   OldOptions, not_defined),
    OldLoc  = maps:get(local,      OldOptions),
    OldEnc  = maps:get(encrypted,  OldOptions),


    Size = proplists:get_value(size,       NewOptions, OldSize),
    MaxF = proplists:get_value(maxNoFiles, NewOptions, OldMaxF),
    Head = proplists:get_value(header,     NewOptions, OldHead),
    Zip  = proplists:get_value(zip,        NewOptions, OldZip),
    Seq  = proplists:get_value(seqNo,      NewOptions, OldSeq),
    Line = proplists:get_value(line,       NewOptions, OldLine),
    Sev  = proplists:get_value(severity,   NewOptions, OldSev),
    Loc  = proplists:get_value(local,      NewOptions, OldLoc),
    Encr = proplists:get_value(encrypted,  NewOptions, OldEnc),

    UpdatedOptions = [{size,       Size},
		      {maxNoFiles, MaxF},
		      {header,     Head},
		      {zip,        Zip},
		      {seqNo,      Seq},
		      {line,       Line},
		      {severity,   Sev},
		      {local,      Loc},
		      {encrypted,  Encr}],	  
    

    case validate_options(UpdatedOptions) of
	{ok, MapOptions} -> 
	    {ok, 
	     logRamLog:set_options(Pid, Name, MapOptions), 
	     {MapOptions, Pid, Ref}};
	Error   -> Error
    end;
handle_set_options(undefined, Name, _) ->
    {error, {log_not_found, Name}}.



%%========================================================================
%% validate_options
%%========================================================================
validate_options(Options) ->
    Size = proplists:get_value(size,       Options, ?DEF_SIZE),
    MaxF = proplists:get_value(maxNoFiles, Options, ?DEF_MAX_FILES),
    Head = proplists:get_value(header,     Options, undefined),
    Zip  = proplists:get_value(zip,        Options, true),
    Seq  = proplists:get_value(seqNo,      Options, true),
    Line = proplists:get_value(line,       Options, true),
    Sev  = proplists:get_value(severity,   Options, not_defined),
    Loc  = proplists:get_bool(local, Options),
    Encr = proplists:get_bool(encrypted, Options),
    Opts = [{size, {Size, MaxF}}, 
	    {head, Head},
	    {zip,  Zip},
	    {seq,  Seq},
	    {sev,  Sev},
	    {line, Line},
	    {loc,  Loc},
	    {encr, Encr}],
    Res  = [R || R <- [vo(K, V, Zip) || {K, V} <- Opts], R /= ok],
    OptMap = #{size       => Size,
	       maxNoFiles => MaxF,
	       header     => Head,
	       zip        => Zip,
	       sequence   => Seq,
	       line       => Line,
	       local      => Loc,
	       encrypted  => Encr},
    case Res of
	[] when Sev == not_defined ->
	    {ok, OptMap}; 
	[] ->
	    {ok, OptMap#{severity => Sev}}; 
	[Error | _] -> 
	    Error
    end.


%%----------------------------------------------------
%% special cases if options are defined in ramlog.cmds 
%% see check_opts/2
%%----------------------------------------------------

vo(_, not_defined, _) ->
    ok;
vo(_, {Size, MaxF}, _) when Size == not_defined orelse 
			    MaxF == not_defined  ->
    ok;
%%----------------------------------------------------
%% size and max number of files
%%----------------------------------------------------
vo(size,{Size, MaxF}, Zip) 
  when is_integer(Size) andalso 
       is_integer(MaxF) ->
    vo_size(Size * MaxF, Size, MaxF, Zip);
vo(size, {Size, MaxF}, _) when is_integer(Size) ->
    {error, lists:flatten(io_lib:format("Max number of files is"
					" not an integer ~p",
					[MaxF]))};
vo(size, {Size, _},_) ->
    {error, lists:flatten(io_lib:format("Size is not an integer ~p",
					[Size]))};
%%----------------------------------------------------
%% head
%%----------------------------------------------------
vo(head, Head, _) when Head == undefined orelse
		       is_function(Head) orelse
		       (is_tuple(Head) andalso size(Head) == 3) ->
    ok;
vo(head, Head, _) ->
    {error, lists:flatten(io_lib:format("Head is not a function or MFA ~p",
					[Head]))};
%%----------------------------------------------------
%% zip
%%----------------------------------------------------
vo(zip, Zip, _) when is_boolean(Zip) ->    
    ok;
vo(zip, Zip, _) ->
    {error, lists:flatten(io_lib:format("Zip is not boolean ~p",
					[Zip]))};
%%----------------------------------------------------
%% sequnce numbering
%%----------------------------------------------------
vo(seq, Seq, _) when is_boolean(Seq) ->    
    ok;
vo(seq, Seq, _) ->
    {error, lists:flatten(io_lib:format("Sequence is not boolean ~p",
					[Seq]))};
%%----------------------------------------------------
%% line numbers
%%----------------------------------------------------
vo(line, Line, _) when is_boolean(Line) ->    
    ok;
vo(line, Line, _) ->
    {error, lists:flatten(io_lib:format("Line is not boolean ~p",
					[Line]))};

%%----------------------------------------------------
%% severity
%%----------------------------------------------------
vo(sev, Sev, _) 
  when is_integer(Sev) andalso 
       Sev < 10 andalso 
       Sev > 0 ->    
    ok;
vo(sev, Sev,_) ->
    {error, lists:flatten(io_lib:format("Invalid severity  ~p",
					[Sev]))};

%%----------------------------------------------------
%% Local or central storage
%%----------------------------------------------------
vo(loc, _Bool, _) ->
    ok;

%%----------------------------------------------------
%% Encrypted
%%----------------------------------------------------
vo(encr, _Bool, _) ->
    ok.

%%----------------------------------------------------
%% Size zipped  
%%----------------------------------------------------
vo_size(TotalSize, _, _, true) when TotalSize > ?MAX_ZIP_TOTAL_SIZE ->
    {error, lists:flatten(io_lib:format("Too much memory requested ~p > ~p (zip)",
					[TotalSize, ?MAX_ZIP_TOTAL_SIZE]))};
vo_size(_, Size, _, true) when Size > ?MAX_ZIP_FILE_SIZE ->
    {error, lists:flatten(io_lib:format("Too biq file size ~p > ~p (zip)",
					[Size, ?MAX_ZIP_FILE_SIZE]))};
vo_size(_, _, MaxF, true) when MaxF > ?MAX_ZIP_NOOF_FILES ->
    {error, lists:flatten(io_lib:format("Too many files ~p > ~p (zip)",
					[MaxF, ?MAX_ZIP_NOOF_FILES]))};
%%----------------------------------------------------
%% Size, not compressed files  
%%----------------------------------------------------
vo_size(TotalSize, _, _, false) when TotalSize > ?MAX_TOTAL_SIZE ->
    {error, lists:flatten(io_lib:format("Too much memory requested ~p > ~p (non zip)",
					[TotalSize, ?MAX_TOTAL_SIZE]))};
vo_size(_, Size, _, false) when Size > ?MAX_FILE_SIZE ->
    {error, lists:flatten(io_lib:format("Too biq file size ~p > ~p (non zip)",
					[Size, ?MAX_FILE_SIZE]))};
vo_size(_, _, MaxF, false) when MaxF > ?MAX_NOOF_FILES ->
    {error, lists:flatten(io_lib:format("Too many files ~p > ~p (non zip)",
					[MaxF, ?MAX_NOOF_FILES]))};
%%----------------------------------------------------
%% Size, OK
%%----------------------------------------------------
vo_size(_, _, _, _) ->
    ok.



%%========================================================================
%% check_opts
%%========================================================================
check_opts(Options, Name) ->
    co_cmds(file:consult(sysEnv:dev_patches_dir() ++ "/ramlog.cmds"),
	    Options,
	    Name).

co_cmds({error, _}, Options, _Name) ->
    Options;
co_cmds({ok, Cmds}, Options, Name) ->
    co_opts(proplists:get_value(Name, Cmds), Options, Name).


co_opts(undefined, OldOptions, _) ->
    OldOptions;
co_opts(NewOptions, OldOptions, Name) ->
    co_validate_options(NewOptions, OldOptions, Name).


    

co_validate_options(NewOptions, OldOptions, Name) ->
    OldSize = maps:get(size,       OldOptions),
    OldMaxF = maps:get(maxNoFiles, OldOptions),
    OldHead = maps:get(header,     OldOptions),
    OldZip  = maps:get(zip,        OldOptions),
    OldSeq  = maps:get(sequence,   OldOptions),
    OldSev  = maps:get(severity,   OldOptions, not_defined),
    OldLine = maps:get(line,       OldOptions),
    OldLoc  = maps:get(local,      OldOptions),
    OldEnc  = maps:get(encrypted,  OldOptions),

    Size = proplists:get_value(size,       NewOptions, OldSize),
    MaxF = proplists:get_value(maxNoFiles, NewOptions, OldMaxF),
    Head = proplists:get_value(header,     NewOptions, OldHead),
    Seq  = proplists:get_value(seqNo,      NewOptions, OldSeq),
    Sev  = proplists:get_value(severity,   NewOptions, OldSev),
    Line = proplists:get_value(sline,      NewOptions, OldLine),
    Zip  = proplists:get_value(zip,        NewOptions, OldZip),
    Loc  = proplists:get_value(local,      NewOptions, OldLoc),
    Enc  = proplists:get_value(encrypted,  NewOptions, OldEnc),

    ValidateZip = co_vo_updated_zip(proplists:get_value(zip, NewOptions), OldZip),
    Opts = [{size, {Size, MaxF}}, 
	    {head, Head},
	    {zip,  ValidateZip},
	    {seq,  Seq},
	    {sev,  Sev},
	    {line, Line},
	    {loc,  Loc},
	    {encr, Enc}],
    Res  = [R || R <- [vo(K, V, ValidateZip) || {K, V} <- Opts], R /= ok],

    case Res of
	[] -> 
	    #{size       => Size,
	      maxNoFiles => MaxF,
	      header     => Head,
	      zip        => Zip,
	      sequence   => Seq,
	      severity   => Sev,
	      line       => Line,
	      local      => Loc,
	      encrypted  => Enc};
	[Error | _] -> 
	    sysInitI:info_msg("~p New options for RAM Log ~p invalid~n"
			      "  Error   = ~p~n"
			      "  Options = ~p~n",
			      [?MODULE, Name, Error, NewOptions]),
	    OldOptions
    end.


%% This is a special case used when testing and not wanting zip:ed files
co_vo_updated_zip(false, true) ->
    true;
%% New options indicates that zip:ing should be turned on.
co_vo_updated_zip(true, _) ->
    true;
%% Use the old value
co_vo_updated_zip(_, Zip) ->
    Zip.

%%========================================================================
%% miscelaneous functions
%%========================================================================

check_write_level(Name, Data) ->
    [LowestSev | _] = lists:sort([Sev || {Sev, _, _} <- Data]),
    cwl(mnesia:dirty_read({logRamLog, Name}), LowestSev).

%% Error and warning should always be sent to the server
cwl(_, Severity) when Severity == error orelse
		      Severity == warning ->
    ok;
%% The filter level is higher than the lowest severity in the write attempt
cwl([#logRamLog{severity = Severity}], LowestSeverity) ->
    case LowestSeverity =<Severity of
	true ->
	    ok;
	_ ->
	    {error, level}
    end;
%% Log not found
cwl([], _) ->
    {error, log_not_found};
%% Filter level is kicking in
cwl(_, _) ->
    {error, level}.




format_state(State) ->
    F       = record_info(fields, state),
    [_ | L] = tuple_to_list(State), 
    lists:zip(F,L).
    
handle_print_loop_data(Logs) ->
    maps:fold(fun hpld/3, ok, Logs).

handle_print_loop_data_log(Log, Data) ->
    hpld(Log, Data, ok).

hpld(Key, {_, Pid, _}, Acc) ->
    LogLoop = logRamLog:get_loop_data(Pid),
    io:format("=== ~p ~p ===~n"
	      "~p~n~n",
	      [Key, Pid, LogLoop]),
    Acc.
   
handle_print_logs(Logs) ->
    maps:fold(fun hpl/3, ok, Logs).

handle_print_log(Log, Data) ->
    hpl(Log, Data, ok).

hpl(_Key, {_, Pid, _}, Acc) ->
    logRamLog:print_log(Pid),
    Acc.

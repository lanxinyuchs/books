%% ===========================================================================
%% Copyright (c) Ericsson AB 2010 All rights reserved.
%%
%% The information in this document is the property of Ericsson.
%%
%% Except as specifically authorized in writing by Ericsson, the
%% receiver of this document shall keep the information contained
%% herein confidential and shall protect the same in whole or in
%% part from disclosure and dissemination to third parties.
%%
%% Disclosure and disseminations to the receivers employees shall
%% only be made on a strict need to know basis.
%% ===========================================================================
%% @hidden
%% @author uabesvi
%% @copyright Ericsson AB 2016
%% @doc
%% This module provides interfaces for
%%
%%
%% @end
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R5A/1      2016-02-10 uabesvi     Created
%%% ----------------------------------------------------------
%%%

-module(logRamLog).
-behaviour(gen_server).
-vsn('/main/R5A/R6A/R7A/R8A/3').
-date('2016-12-13').
-author('uabesvi').
-shaid('5ec403a880dd59bfb32e21ae7245dc2143f26d9a').



-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 code_change/3, 
	 terminate/2]).


-export([start/2]).
-export([stop/2]).

-export([write_log/5]).
-export([generate_esi/2]).


%% Debug commands
-export([set_severity/3]).
-export([set_options/3]).
-export([get_loop_data/1]).
-export([print_log/1]).



%% interface
%%-export([init/3]).


-include("log.hrl").



-define(SIZE,      1000).
-define(MAX_FILES, 3).

-define(CALL_TIMEOUT, 10000).

-define(MAX_LEN_SEQ, 10).

-define(DEFAULT_SEVERITY, 1).

%% record definitions.

-record(state, {name,          %% log name
		log_level = 1, %% severity filtering level
		size,          %% size of RM
		max_files,     %% Max number of files on disc
		header,        %% If header should be written in the file
		zip,           %% If the files should be zipped on disc
		seq_no,        %% If sequence numbers should be written
		line_no,       %% If line numbers should be written 
		file_path,     %% File path
		bytes = 0,     %% Current number of bytes in the RAM
		log_tab,       %% The RAM table
		black_tab}).   %% Current black list table

%% ===========================================================================
%% EXPORTED FUNCTIONS
%% ===========================================================================
%% ===========================================================================
%% start
%%
%%       -> ok | {ok, Pid}
%%
%% Start File Log.
%% Spawns a process to handle the log messages.
%%
%% ===========================================================================
%% @hidden
start(Name, LogOptions) ->
    Module     = ?MODULE,
    Args       = {self(), Name, LogOptions},
    GsOptions  = [],
    gen_server:start_link(Module, Args, GsOptions).



%% ===========================================================================
%% stop(Pid) -> ok.
%%
%% Stop the process
%%
%% ===========================================================================
%% @hidden
stop(Pid, Reason) ->
    try
	gen_server:call(Pid, {stop, Reason})
    catch exit:E ->
	    {error, E}
    end.

write_log(Pid, Name, Tag, Time, Data) ->
    gen_server:cast(Pid, {write_log, {Name, Tag, Time, Data}}).

generate_esi(Pid, Name) ->
    try
	gen_server:call(Pid, generate_esi, 25000)
    catch
	exit:_ ->
	    sysInitI:warning_msg("~p: Timeout while generating esi for log ~p~n", 
				 [?MODULE, Name]),
	    {error, {timeout, {?MODULE, generate_esi}}}
    end.

set_severity(Pid, _Name, LogLevel) ->
    try
	gen_server:call(Pid, {set_severity, LogLevel})
    catch exit:E ->
	    {error, E}
    end.

set_options(Pid, _Name, Options) ->
    try
	gen_server:call(Pid, {set_options, Options})
    catch exit:E ->
	    {error, E}
    end.

get_loop_data(Pid) ->
    try
	gen_server:call(Pid, get_loop_data)
    catch exit:E ->
	    {error, E}
    end.

print_log(Pid) ->
    try
	gen_server:call(Pid, print_log)
    catch exit:E ->
	    {error, E}
    end.


%%========================================================================
%% handle_call(Msg, From, State) -> {reply, Res, State}
%%========================================================================
handle_call(generate_esi, _From, State) ->
    {Res, NewState} = write_file(true, State),
    {reply, Res, NewState};

handle_call({stop, Reason}, _From, State) ->
    Msg = io_lib:format("Process stopped.~n  Reason = ~p~n", [Reason]),
    State2 = handle_write_log({logRamLog, ?LINE, self()},
			      os:timestamp(),
			      {?DEFAULT_SEVERITY, Msg, false},
			      State),
    
    {Res, NewState} = write_file(true, State2),
    {stop, stop_reason(Reason), Res, NewState};

handle_call({set_severity, NewLevel}, _From, #state{name = Name} = State) ->
    write_severity(Name, NewLevel),
    {reply, ok, State#state{log_level = NewLevel}};

handle_call({set_options, Options}, _From, #state{name = Name} = State) ->
    NewState = handle_set_options(Options, State),
    Severity = maps:get(severity, Options, undefined), 
    write_severity(Name, Severity),
    {reply, ok, NewState};

handle_call(get_loop_data, _From, State) ->
    {reply, format_loop(State), State};

handle_call(print_log, 
	    _From, 
	    #state{name      = Name,
		   log_tab   = LogTab,
		   black_tab = BlTab} = State) ->
    print_log(Name, LogTab, BlTab),
    {reply, ok, State};



handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


%%========================================================================
%% handle_cast(Msg, State) -> {noreply, State}
%%========================================================================
handle_cast({write_log, {Name, Tag, Time, Data}},
	    #state{name      = Name,
		   log_level = Level,
		   size      = MaxSize,
		   bytes     = Bytes} = State) ->
    State2 = handle_write_log(Tag,
			      Time,
			      get_current_data(Data, Level),
			      State),
    {_, State3} = write_file(Bytes > MaxSize * 1000, State2),
    {noreply, State3};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%========================================================================
%% handle_info(Info, State) -> {noreply, State}
%%========================================================================
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
terminate(Reason, #state{}) ->
    Reason.



%% ===========================================================================
%% init(Creator, Name, Options)
%%
%% The function gets executed in a process initiated by start.
%% Creates a file in
%%
%% ===========================================================================
init({_Creator, Name, Options}) ->
    Size     = maps:get(size, Options),
    MaxF     = maps:get(maxNoFiles, Options),
    Head     = maps:get(header, Options),
    Zip      = maps:get(zip, Options),
    SeqNo    = maps:get(sequence, Options),
    LogLevel = maps:get(severity, Options, ?DEFAULT_SEVERITY),
    LineNo   = maps:get(line, Options),
    %% FilePath = filename:join([sysEnv:rcs_dir(), "log", Name]),
    FilePath = logLib:log_dir(Name),
    LogTab   = ets:new(measured, [ordered_set]),
    BlTab    = ets:new(measured, [ordered_set]),
    case init_ensure_dir(filelib:ensure_dir(FilePath ++ "/ignored")) of
	ok ->
	    logI:register_esi_dir(FilePath),
	    write_severity(Name, LogLevel),
	    {ok, #state{name      = Name,
			log_level = LogLevel,
			size      = Size,
			max_files = MaxF,
			header    = Head,
			zip       = Zip,
			seq_no    = choose(SeqNo == true, 1, false),
			line_no   = LineNo,
			file_path = FilePath,
			log_tab   = LogTab,
			black_tab = BlTab}};
	Error ->
	    {stop, Error}
    end.


init_ensure_dir(ok) ->
    ok;
init_ensure_dir(Error) ->
    sysInitI:info_msg("Failed to create file directory ~p~n", [Error]),
    Error.




%% ===========================================================================
%% INTERNAL FUNCTIONS
%% ===========================================================================


%% ===========================================================================
%% handle_write_log(Tag, Time, Data, State) -> State
%%
%% Tag       = {Module, Line, Pid}
%% Module    = atom()
%% Line      = integer()
%% Pid       = pid()
%% Time      = os:timestamp()
%% Data      = {Severity, Msg} | {Severity, Msg, BlackList}
%% Severity  = integer() | string()
%% Msg       = string()
%% BlackList = boolean()
%% State     = #state{}
%%
%%
%% ===========================================================================
handle_write_log(_Tag,
		 _Time,
		 {Severity, _Msg, _BlackList},
		 #state{log_level = Level} = State)
  when is_integer(Severity) andalso Severity > Level ->
    State;
handle_write_log(Tag,
		 Time,
		 {Severity, Msg, false},
		 State) ->
    hwl({Tag, Time, Severity, Msg}, false, "", State);
handle_write_log(Tag,
		 Time,
		 {Severity, Msg, BlackList},
		 #state{black_tab = BlackTab} = State) ->
    BL = hwl_black_listed(ets:member(BlackTab, Tag), BlackList, BlackTab, Tag),
    case BL of
	true  -> hwl({Tag, Time, Severity, Msg}, BL, BlackList, State);
	false -> State
    end.


%%-------------------------------------------------------------
%% hwl(Data, BlackListed, State) -> State
%%
%% write the entry to the log
%%-------------------------------------------------------------
hwl(Data,
    BlackListed,
    BlackList,
    #state{log_tab = LogTab,
	   seq_no  = SeqNo,
	   line_no = LineNo,
	   bytes   = Bytes} = State) ->
    {Bin, Len} = hwl_format(Data, 
			    BlackListed, 
			    LineNo, 
			    hwl_bl_to_str(BlackList),
			    SeqNo),
    ets:insert(LogTab, {Bin}),
    case SeqNo of
	false -> 
	    State#state{bytes = Bytes + Len};
	_     ->
	    State#state{seq_no = SeqNo + 1,
			bytes  = Bytes + Len}
    end.


hwl_bl_to_str(Int)  when is_integer(Int) -> integer_to_list(Int); 
hwl_bl_to_str(Atom) when is_atom(Atom)   -> atom_to_list(Atom);
hwl_bl_to_str(Str)                       -> Str.

%%-------------------------------------------------------------
%% hwl_format(...) -> {LogEntry, Length}
%%
%% LogEntry = binary()
%% Length   = integer()
%%
%% Create the log entry
%%-------------------------------------------------------------
hwl_format({{Module, Line, Pid}, Time, Severity, Info},
	   BlackListed,
	   LineNo, 
	   BL,
	   SeqNo) ->
    List =
	get_seq_no(SeqNo) ++
	get_time(Time) ++ "Z " ++
	pid_to_list(Pid) ++ " " ++
	atom_to_list(Module) ++ 
	choose(LineNo, ":" ++ integer_to_list(Line) ++ " ", " ") ++
	"S:" ++ hwl_severity(Severity) ++
	choose(BlackListed, " BL:" ++ BL ++ "\n", "\n") ++
	Info ++ "\n",
    {list_to_binary(List), length(List)}.

%%-------------------------------------------------------------
%% hwl_severity(Severity) -> SeverityString
%%
%% Transform severity to log format
%%-------------------------------------------------------------
hwl_severity(Severity) when is_integer(Severity) ->
    integer_to_list(Severity);
hwl_severity(Severity) when is_atom(Severity) ->
    string:to_upper(atom_to_list(Severity));
hwl_severity(Severity) when is_list(Severity) ->
    string:to_upper(Severity);
hwl_severity(_Severity) ->
    "UNVALID_SEVERITY".


%%-------------------------------------------------------------
%% hwl_black_listed() -> ok
%%-------------------------------------------------------------
%% not yet in the black list
hwl_black_listed(false, file, BlackTab, Tag) ->
    ets:insert(BlackTab, {Tag, file}),
    true;
%% not yet in the black list
hwl_black_listed(false, Sec, BlackTab, Tag) ->
    ets:insert(BlackTab, {Tag, hwl_get_bl_time(Sec)}),
    true;
%% not yet in the black list
%% check if quarantine has expired
hwl_black_listed(true, BlackList, BlackTab, Tag) ->
    hwl_bl(ets:lookup(BlackTab, Tag),
	   BlackList,
	   BlackTab,
	   Tag,
	   os:timestamp()).

%%----------------------------
%% hw_get_bl_time() ->
%%----------------------------
%% type is file, i.e. only one entry per file is allowed
hwl_bl([{Tag, file}], _, _BlackTab, Tag, _Now) ->
    false;
%% time has expired, write the entry in the log and
%% put it back into quarantine
hwl_bl([{Tag, Time}], BlackList, BlackTab, Tag, Now) when Time < Now ->
    ets:delete(BlackTab, Tag),
    ets:insert(BlackTab, {Tag, hwl_get_bl_time(BlackList)}),
    true;
hwl_bl(_, _, _, _, _) ->
    false.

%%----------------------------
%% hwl_get_bl_time() -> Time
%% get quarantine time
%%----------------------------
hwl_get_bl_time(file) ->
    file;
hwl_get_bl_time(AddSec) ->
    {Mega, Sec, _} = os:timestamp(),
    Time = Mega * 1000000 + Sec + AddSec + 1,
    {Time div 1000000, Time rem 1000000, 0}.




%% ===========================================================================
%% write_file(Write, State) -> {Result, State}
%%
%% Write  = boolean()
%% State  = #state{}
%% Result = ok | {error, Reason}
%% ===========================================================================
write_file(false, State) ->
    {ok, State};
write_file(true, #state{bytes = 0} = State) ->
    {ok, State};
write_file(true,
	   #state{name      = Name,
		  file_path = Path,
		  max_files = MaxFiles,
		  log_tab   = LogTab,
		  black_tab = BlackTab,
		  header    = Header,
		  zip       = Zip} = State) ->
    
    Post = get_time(os:timestamp()),
    File = lists:append([Path, "/", Name, "_", Post, ".txt"]),
    Data = ets:select(LogTab, [{{'$1'}, [], ['$1']}]),

    Files = wf_get_files(filelib:wildcard(Path ++ "/" ++ Name ++ "_*"), []),
    wf_delete_files(Files, MaxFiles, Path),

    Entry = iolist_to_binary([wf_get_header(Header), "\n", Data]),
    {ZipFile, ZipData} = wf_zip(Zip, File, Entry),

    Res = wf_write(file:write_file(ZipFile, ZipData),
		   sysEnv:rcs_mode_2()),
    ets:delete_all_objects(LogTab),
    ets:match_delete(BlackTab, {'_', file}),
    {Res, State#state{bytes  = 0}}.


wf_get_files([], Acc) ->
    lists:sort(Acc);
wf_get_files([H | T], Acc) ->
    [Name | _] = lists:reverse(string:tokens(H, "/")),
    wf_get_files(T, [Name | Acc]).
    

wf_zip(true, Name, Data) ->
    {Name ++ ".gz", zlib:gzip(Data)};
wf_zip(false, Name, Data) ->
    {Name, Data}.


wf_write(ok, _) ->
    ok;
wf_write(Error, vrcs) ->
    sysInitI:warning_msg("Failed to write to file: ~p~n", [Error]),
    Str = lists:flatten(io_lib:format("~p~n", [Error])),
    appmI:restart_node(cold,
		       atom_to_list(?MODULE) ++ " file write error: " ++ Str),
    Error;
wf_write(Error, _) ->
    sysInitI:warning_msg("Failed to write to file: ~p~n", [Error]),
    Error.


wf_delete_files([Oldest | _] = Files, MaxFiles, Path)
  when length(Files) >= MaxFiles ->
    file:delete(filename:join(Path, Oldest)),
    ok;
wf_delete_files(_, _MaxFiles, _) ->
    ok.


wf_get_header(Fun) when is_function(Fun) ->
    Fun();
wf_get_header({M, F, A}) ->
    apply(M, F, A);
wf_get_header(_Head) ->
    [].




get_current_data([{Severity, Msg, BL}], _Level) ->
    {Severity, gcd_format_msg(Msg), BL};
get_current_data([{error = Severity, Msg, BL} | _T], _Level) ->
    {Severity, gcd_format_msg(Msg), BL};
get_current_data([{Severity, Msg, BL} | T], Level) ->
    %% gcd(T, Level, {Severity, gcd_format_msg(Msg), BL}).
    gcd(T, Level, {Severity, Msg, BL}).

gcd([{warning = Severity, Msg, BL}], _Level, _Res) ->
    {Severity, gcd_format_msg(Msg), BL};
gcd([{Severity, _Msg, _BL} | _], Level, {Sev, Msg, BL})
  when Severity > Level ->
    {Sev, gcd_format_msg(Msg), BL};
gcd([{Severity, Msg, BL} | T], Level, _Res) ->
    %% gcd(T, Level, {Severity, gcd_format_msg(Msg), BL});
    gcd(T, Level, {Severity, Msg, BL});
gcd([], _, {Sev, Msg, BL}) ->
    {Sev, gcd_format_msg(Msg), BL}.


gcd_format_msg({Str, Arg}) ->
    lists:flatten(io_lib:format(Str, Arg));
gcd_format_msg(Fun) when is_function(Fun) ->
    gcd_format_msg(Fun());
gcd_format_msg(Str) ->
    lists:flatten(io_lib:format(Str, [])).


%%===========================================================================
%% handle_set_options(Options, State) -> State
%%
%% update options
%%===========================================================================
handle_set_options(Options,
		   #state{seq_no    = OldSeqNo,
			 log_level = OldLogLevel} = State) ->
    Size     = maps:get(size, Options),
    MaxF     = maps:get(maxNoFiles, Options),
    Head     = maps:get(header, Options),
    Zip      = maps:get(zip, Options),
    SeqNo    = maps:get(sequence, Options),
    LogLevel = maps:get(severity, Options, OldLogLevel),
    LineNo   = maps:get(line, Options),

    case (OldSeqNo == false) and (SeqNo == true) of
      true ->
        NewSeqNo = 1;
      _ ->
        NewSeqNo = choose(SeqNo, OldSeqNo, false)
    end,
    
    State#state{size      = Size,
		max_files = MaxF,
		header    = Head,
		zip       = Zip,
		seq_no    = NewSeqNo,
		line_no   = LineNo,
		log_level = LogLevel}.


%%===========================================================================
%% write_severity(Name, Severity)
%%
%% write the severity level into the DB
%%===========================================================================
write_severity(_Name, undefined) ->
    ok;
write_severity(Name, Severity) ->
    Obj = #logRamLog{name     = Name,
		     severity = Severity},
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Obj) end).


%%===========================================================================
%% get_time(GP) -> string()
%%
%% get a printable time
%%===========================================================================
get_time({_, _, MS} = Now) ->
    get_time(calendar:now_to_local_time(Now), MS div 1000).

get_time({{Y, M, D}, {H, Mi, S}}, MS) ->
    lists:append([integer_to_list(Y), "-",
		  gt_zero(M),
		  integer_to_list(M), "-",
		  gt_zero(D),
		  integer_to_list(D), "T",
		  gt_zero(H),
		  integer_to_list(H), ":",
		  gt_zero(Mi),
		  integer_to_list(Mi), ":",
		  gt_zero(S),
		  integer_to_list(S), ".",
		  gt_zero_ms(MS),
		  integer_to_list(MS)]).

gt_zero(X) when X < 10 -> "0";
gt_zero(_)             -> "".

gt_zero_ms(X) when X < 10     -> "00";
gt_zero_ms(X) when X < 100    -> "0";
gt_zero_ms(_)                 -> "".


get_seq_no(Int) when is_integer(Int)  ->
    string:right(integer_to_list(Int), ?MAX_LEN_SEQ, $.) ++ " ";
get_seq_no(_) ->
    "".

stop_reason(deleted)    -> normal; 
stop_reason(terminated) -> normal;
stop_reason(Else)       -> Else.


choose(true,  A, _) -> A;
choose(false, _, B) -> B.


format_loop(State) ->
    F       = record_info(fields, state),
    [_ | L] = tuple_to_list(State),
    lists:zip(F,L).


print_log(Name, Tab, BL) ->
    io:format("~n===== ~p =====~n~p~n~n"
	      "---- Black list ----~n~p~n~n"
	      "---- Tab size   ----~n~p~n~n",
	      [Name,
	       ets:select(Tab, [{{'$1'}, [], ['$1']}]),
	       ets:tab2list(BL),
	       ets:info(Tab, memory)]).

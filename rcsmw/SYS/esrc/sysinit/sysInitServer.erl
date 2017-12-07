%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysInitServer.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R7A/R8A/R10A/2

%%% @doc ==The sysInit service==
%%% This module implements the sysInit service, which is active in the very
%%% beginning of a system startup, before mnesia, for example.

-module(sysInitServer).
-behaviour(gen_server).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R7A/R8A/R10A/2').
-date('2017-05-29').
-author('etxjotj').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% R3A/1      2015-01-13 etxpeno     Only create mnesia schema on active MP
%%% R3A/2      2015-02-04 etxlg       Enable introduction of net_ns on LMT
%%% -----      -------    --------    ------------------------
%%% R4A/1      2015-07-20 etxjotj     Disk statistics during startup
%%% R4A/2      2015-08-21 etxjotj     Mnesia on tmp
%%% R4A/3      2015-08-27 etxjotj     Error logger
%%% R4A/5      2015-09-01 etxjotj     Friendly reminder support
%%% R4A/7      2015-09-03 etxjotj     Raised messageLimit to 20
%%% R4A/8      2015-10-01 etxjotj     Usage of autobackups at reboot
%%% -----      -------    --------    ------------------------
%%% R5A/1      2015-10-06 etxjotj     Removed erlang:now
%%% R5A/2      2015-10-19 etxpejn     Don't run prep_mnesia_normal on regular DU
%%% R5A/3      2015-11-02 etxtory     Added *_msg; so regular can
%%%                                   can print on core node.
%%%                                   Merged R4A/10
%%% R5A/4      2015-11-25 etxpeno     Using monotonic clock in get_time/0
%%% R5A/5      2015-12-10 etxpeno     support for cover compiling
%%% R5A/6      2016-03-11 erarafo     Improved logging of restart_logger trouble
%%% R5A/7      2016-03-12 erarafo     Fixed fatal bad call in R5A/6
%%% R5A/8      2016-03-21 etxberb     Added truncate_report functions.
%%% R5A/10     2016-03-28 etxberb     Enhanced truncate_report functions.
%%% -----      -------    --------    ------------------------
%%% R7A/1      2016-10-15 etxjotj     VRCS backup restore
%%% ----------------------------------------------------------
%%% R8A/1      2016-11-03 emariad     SP341: Decrypt DB for cloud
%%% R8A/2      2017-01-24 etxberb     Added stPh_preInit.
%%% ----------------------------------------------------------
%%% R10A/1     2017-05-22 etxjotj     Added sanity check for autobackup
%%% R10A/2     2017-05-29 etxjotj     Improved sanity check for autobackup
%%% ----------------------------------------------------------

-include("sys.hrl").

%% Basename of C executable that performs logging to the trace9 group.
-define(RESTART_LOGGER_PORT_PROGRAM, "restart_logger").
-define(CUP_BINARY, "cup").
-define(NS_PATH, <<"/run/netns">>).
-define(LMT_NS, <<"LMT_net_ns">>).

%% Timeout for messages to the port program, slightly less than the
%% gen_server default timeout.
-define(RESTART_LOGGER_TIMEOUT, 4000).

%% Enabling this macro causes an INFO report to be written to the
%% Erlang log when the port program is stopped.
-define(STOP_VERBOSE(MESSAGE),
    info({?MODULE, {MESSAGE, ?RESTART_LOGGER_PORT_PROGRAM}})).
%%-define(STOP_VERBOSE(MESSAGE), ok).

%% Enabling this macro causes the restart_logger port program to write
%% progress messages to sysEnv:rcs_dir().
%%-define(RESTART_LOGGER_DEBUG_DIR, [{"DEBUG_LOG_DIR", sysEnv:rcs_dir()}]).
-define(RESTART_LOGGER_DEBUG_DIR, []).
-define(SERVER, ?MODULE).


%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/0, stop/0]).

-export([restart_complete/0, restart_type/0]).

-export([restart_logger_trace/3, restart_logger_stop/2]).

-export([is_secure/0]).
-export([get_lmt_ns/0]).

-export([mnesia_dir/0, mnesia_tmp_dir/0]).

-export([make_msg/1, make_report/1]).
-export([error_msg_node/3]).
-export([info_msg_node/3]).
-export([warning_msg_node/3]).

-export([lift_quarantine/0, lift_quarantine/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     code_change/3, terminate/2]).

-define(DBG_CMDS, filename:join(sysEnv:dev_patches_dir(),"debug.cmds")).
-define(SYS_MAX_MSG_SIZE, 10000).
-define(SYS_MAX_MSG_LEN,  250).

-define(Marker_RemovedInfo, '<===NO OF ELEMENTS REMOVED===').
-define(Marker_RemovedInfo(NoOfElems), {?Marker_RemovedInfo, NoOfElems}).

%% Usage of error_logger:XXX_report
-define(LOG_ERR(__ReportInfo),
	try
	    sysInitI:error_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:error_report(?RepInfo(__ReportInfo))
	end).
-define(LOG_INFO(__ReportInfo),
	try
	    sysInitI:info_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:info_report(?RepInfo(__ReportInfo))
	end).
-define(LOG_WARN(__ReportInfo),
	try
	    sysInitI:warning_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:warning_report(?RepInfo(__ReportInfo))
	end).
-define(RepInfo(__RepInfo),
	[{?MODULE, ?FUNCTION} | __RepInfo]).
%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).
-define(MODULE_STR, atom_to_list(?MODULE)).
-define(MonoTime, erlang:monotonic_time()).
-define(PROC_INFO(Pid), sysUtil:pid_info(Pid, {all, [error_handler]})).
-define(STATE_INFO(Record),
	sysUtil:record_format(record_info(fields, state), Record)).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

start() ->
    gen_server:start_link({local, sysInitServer}, ?MODULE, [], []).

stop() ->
    try gen_server:call(?SERVER, stop) of
    _ ->
        ok
    catch
    _:_ -> ok
    end.


-spec restart_type() -> vm_restart | local.

restart_type() ->
    gen_server:call(sysInitServer, restart_type).


%%%-----------------------------------------------------------------
%%% @doc Sets the restart type to 'local'. This happens when
%%% all applications included with 'sys' have got the 'activate'
%%% message.
%%% @end
%%%-----------------------------------------------------------------

restart_complete() ->
    gen_server:cast(sysInitServer, restart_complete).

restart_logger_trace(Module, Line, Message) ->
    gen_server:cast(?SERVER, {restart_logger_trace, Module, Line, Message}).

restart_logger_stop(Module, Line) ->
    gen_server:cast(?SERVER, {restart_logger_stop, Module, Line}).

%%%-----------------------------------------------------------------
%%% @doc Report if the board is secure.
%%%  The reply is based on the presence of VC. The function will
%%%  true if the board has a Vendor Credential, false othervise.
%%% @end
%%%-----------------------------------------------------------------
-spec is_secure() -> boolean().
is_secure() ->
    gen_server:call(?SERVER, is_secure).

%%%-----------------------------------------------------------------
%%% @doc Return the network namespace of the LMT port
%%% @end
%%%-----------------------------------------------------------------
-spec get_lmt_ns() -> binary().
get_lmt_ns() ->
    gen_server:call(?SERVER, get_lmt_ns).

%%%-----------------------------------------------------------------
%%% @doc Send a error_logger message through quarantine check
%%% @end
%%%-----------------------------------------------------------------
make_msg({MsgType, Mfa, Format, Args}) ->
    Func = case MsgType of
           error -> error_msg;
           info -> info_msg;
           warning -> warning_msg
       end,
    case is_message_to_big(Format, Args) of
    true ->
        sysInitI:warning_msg("Withheld long ~w from~n  ~p~n",
                 [Func, Mfa]);
    false ->
        case is_allowed_to_log(Mfa) of
        true ->
            error_logger:Func("~w~n"++Format, ['$sysErrorLogger'|Args]);
        false ->
            ok
        end
    end.

force_info_msg(Format, Args) ->
    error_logger:info_msg("~w~n"++Format, ['$sysErrorLogger'|Args]).

%%%-----------------------------------------------------------------
%%% @doc Send a error_logger report through quarantine check
%%% @end
%%%-----------------------------------------------------------------
make_report({ReportType, Mfa, Type, Report}) ->
    Func =
    case ReportType of
        error -> error_report;
        info -> info_report;
        warning -> warning_report
    end,
    case is_allowed_to_log(Mfa) of
    true ->
        log_report(Mfa, Type, Report, Func);
    false ->
        ok
    end.

%%%-----------------------------------------------------------------
log_report(Mfa, Type, Report, Func) ->
    RepLen = length(Report),
    if
    RepLen =< ?SYS_MAX_MSG_LEN ->
        case is_report_too_big(Report) of
        false ->
            %% Normal case
            error_logger:Func(Type, ['$sysErrorLogger' | Report]);
        true ->
            {ReportHead, ReportTail} =
            lists:split(length(Report) div 2, Report),
            {TruncRepHead, TruncRepTail} =
            truncate_report(ReportHead, ReportTail),
            TruncReport = TruncRepHead ++ TruncRepTail,
            log_report_trunc(Mfa, Type, TruncReport, Func)
        end;
    ?ELSE ->
        HalfMaxLen = ?SYS_MAX_MSG_LEN div 2,
        {ReportHead, Tail} = lists:split(HalfMaxLen, Report),
        CntMidRemoved = length(Tail) - HalfMaxLen,
        ReportTail = lists:nthtail(CntMidRemoved, Tail),
        {TruncRepHead, TruncRepTail} =
        truncate_report(ReportHead, ReportTail),
        TruncReport =
        TruncRepHead ++
        [?Marker_RemovedInfo(CntMidRemoved)] ++
        TruncRepTail,
        log_report_trunc(Mfa, Type, TruncReport, Func)
    end,
    ok.

log_report_trunc(Mfa, Type, TruncReport, Func) ->
    Ref = erlang:make_ref(),
    TooBigString = "===Too big REPORT",
    error_logger:Func(Type, ['$sysErrorLogger' | TruncReport ++
                 [{TooBigString ++ ", see also", Ref}]]),
    sysInitI:warning_report([{Ref, "Truncated"},
                 {TooBigString ++" from", Mfa}]).

%%%-----------------------------------------------------------------
truncate_report(ReportHead, ReportTail) ->
    truncate_report(ReportHead, lists:reverse(ReportTail), [], []).

%%%-----------------------------------------------------------------
truncate_report([HE | HTail], [TE | TTail], AccReportHead, AccReportTail) ->
    NewAccReportHead = AccReportHead ++ [HE],
    NewAccReportTail = AccReportTail ++ [TE],
    case is_report_too_big(NewAccReportHead ++ AccReportTail) of
    false ->
        case is_report_too_big(NewAccReportHead ++ NewAccReportTail) of
        false ->
            truncate_report(HTail,
                    TTail,
                    NewAccReportHead,
                    NewAccReportTail);
        true ->
            truncate_report(HTail,
                    TTail,
                    NewAccReportHead,
                    mark_RemovedInfo(AccReportTail))
        end;
    true ->
        case is_report_too_big(AccReportHead ++ NewAccReportTail) of
        false ->
            truncate_report(HTail,
                    TTail,
                    mark_RemovedInfo(AccReportHead),
                    NewAccReportTail);
        true ->
            truncate_report(HTail,
                    TTail,
                    mark_RemovedInfo(AccReportHead),
                    mark_RemovedInfo(AccReportTail))
        end
    end;
truncate_report([], [], AccReportHead, AccReportTail) ->
    {AccReportHead, lists:reverse(AccReportTail)};
truncate_report([], RestTail, AccReportHead, AccReportTail) ->
    {AccReportHead,
     lists:reverse(truncate_report_rest(RestTail, AccReportTail))};
truncate_report(RestHead, [], AccReportHead, AccReportTail) ->
    {truncate_report_rest(RestHead, AccReportHead),
     lists:reverse(AccReportTail)}.

%%%-----------------------------------------------------------------
truncate_report_rest(Rest, AccReport) ->
    NewAccReport = AccReport ++ Rest,
    case is_report_too_big(NewAccReport) of
    false ->
        NewAccReport;
    true ->
        mark_RemovedInfo(AccReport)
    end.

%%%-----------------------------------------------------------------
mark_RemovedInfo(Info) ->
    try lists:last(Info) of
    {?Marker_RemovedInfo, Cnt} ->
        lists:sublist(Info, length(Info) - 1) ++
        [?Marker_RemovedInfo(Cnt + 1)];
    _ ->
        Info ++ [?Marker_RemovedInfo(1)]
    catch
    _ : _ ->
        %% Info == []
        [?Marker_RemovedInfo(1)]
    end.

%%%-----------------------------------------------------------------
%%% @doc Error message to be logged in the erlang.log in Node.
%%% @end
%%%-----------------------------------------------------------------
error_msg_node(Node, Format, Data) ->
    gen_server:call({?SERVER, Node}, {error_msg, Format, Data}).

%%%-----------------------------------------------------------------
%%% @doc Information message to be logged in the erlang.log in Node.
%%% @end
%%%-----------------------------------------------------------------
info_msg_node(Node, Format, Data) ->
    gen_server:call({?SERVER, Node}, {info_msg, Format, Data}).

%%%-----------------------------------------------------------------
%%% @doc Warning message to be logged in the erlang.log in Node.
%%% @end
%%%-----------------------------------------------------------------
warning_msg_node(Node, Format, Data) ->
    gen_server:call({?SERVER, Node}, {warning_msg, Format, Data}).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%% The vc_state member in the state-record has three possible values
%% because in actuality there are three possibilities, the vc exists,
%% the no_vc-flag exists, or something is broken. The cup implementation
%% currently doesn't differentiate.
-record(state, {restart_type=vm_restart    :: vm_restart | local,
        restart_logger_port        :: undefined | port(),
        wd_tref,
        vc_state = unknown     :: vc_state(),
        lmt_ns = <<>>          :: binary()
           }).

init(_) ->
    ets:new(sysErrorLoggerThresholds, [set, public, named_table]),
    ets:new(sysErrorLoggerStats, [set, public, named_table]),

    Path = filename:join(sysEnv:dev_patches_dir(), "diskstats"),
    case filelib:is_file(Path) of
    true ->
        {ok, Fd} = file:open("/tmp/diskstats", [write]),
        put(fd, Fd),
        ets:new(diskstats, [set, public, named_table]),
        {ok, TimerRef} = timer:send_interval(60000, diskstats),
        timer:apply_after(330000, timer, cancel, [TimerRef]);
    false ->
        ok
    end,

    %% WdTref = start_watchdog_kicker(),
    VcState = read_vc_state(os:find_executable(?CUP_BINARY)),
    Lmt_ns = return_lmt_ns(),
    error_logger:add_report_handler(sysInitErrorTTY),
    error_logger:add_report_handler(sysInitSaslTTY),
    error_logger:delete_report_handler(error_logger_tty_h),
    error_logger:delete_report_handler(sasl_report_tty_h),

    %% cover compile all beam files in Apps if simulated and
    %% environment variable COVER is equal to "true"
    Apps = [aic, alh, appm, cec, cert, clh, coi, comsa, ecoli, eitc, eqs, gmf,
        lma, log, omc, oot, pes, pms, swm, sys, sysdb, sysinit, tim],
    cover_compile(sysEnv:ssit(), os:getenv("COVER"), Apps),

    %% read dev_patches/debug.cmds file if it exists
    case file:eval(?DBG_CMDS) of
    ok ->
        error_logger:info_msg("~w: Applying commands in ~p~n",
                  [?MODULE,?DBG_CMDS]);
    _ ->
        ok
    end,

    MpId = clhI:own_mp_id(),
    prep_mnesia(MpId),

    Port = init_restart_logger_port(VcState),

    sysInitI:restart_logger_trace(?MODULE, ?LINE, "middleware starting"),

    InitModules = sysDbServer:get_init_modules(),
    stPh_runSeq(InitModules, stPh_preInit, ?MonoTime),
    {ok, #state{restart_logger_port=Port,vc_state=VcState,lmt_ns=Lmt_ns}}.

%%% ###########################################################################
%%% stPh_runSeq
%%%
%%% ###=====================================================================###
stPh_runSeq(Modules, Func, T0) ->
    stPh_runSeq(Modules, Func, [], T0).

stPh_runSeq([Module | Tail], Func, Results, T0) ->
    Args = [],
    {ok, Result} = stPh_apply(Module, Func, Args),
    stPh_runSeq(Tail, Func, Results ++ [Result], T0);
stPh_runSeq([], Func, Results, T0) ->
    T1 = ?MonoTime,
    ?LOG_INFO([{"Start phase", Func} | Results] ++
	      [{'TOTAL', sysUtil:time_to_string(T1 - T0)}]),
    ok.

%%% ###########################################################################
%%% stPh_apply
%%%
%%% ###=====================================================================###
stPh_apply(Module, Func, Args) ->
    T0 = ?MonoTime,
    try apply(Module, Func, Args) of
	ok ->
	    T1 = ?MonoTime,
	    {ok, {Module, sysUtil:time_to_string(T1 - T0)}};
	Any ->
	    erlang:error({unknown_response, Any}, [Module])
    catch
	error : undef ->
	    case erlang:get_stacktrace() of
		[{Module, _Func, _, _}|_] ->
		    {ok, {Module, undefined}};
		StackTrace ->
		    erlang:error({undef,StackTrace}, [Module])
	    end;
	Type : Reason ->
	    Stack = erlang:get_stacktrace(),
	    io:format("~n"),
	    ?LOG_ERR([{mfa, {Module, Func, Args}},
		      {Type, Reason},
		      Stack]),
	    erlang:Type(Reason, [Module])
    end.

%%% ###########################################################################
handle_call({error_msg, Format, Data}, _, State) ->
    sysInitI:error_msg(Format, Data),
    {reply, ok, State};
handle_call({info_msg, Format, Data}, _, State) ->
    sysInitI:info_msg(Format, Data),
    {reply, ok, State};
handle_call({warning_msg, Format, Data}, _, State) ->
    sysInitI:warning_msg(Format, Data),
    {reply, ok, State};
handle_call(is_secure, _, #state{vc_state = vc} = State) ->
    {reply, true, State};
handle_call(is_secure, _, State) ->
    {reply, false, State};
handle_call(get_lmt_ns, _, State) ->
    {reply, State#state.lmt_ns, State};
handle_call(restart_type, _, State) ->
    {reply, State#state.restart_type, State};
handle_call(stop, _, State) ->
    {stop, normal, ok, State};
handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(restart_complete, State) ->
    sysInitI:restart_logger_trace(?MODULE, ?LINE, "restart complete"),
    {noreply, State#state{restart_type=local}};
handle_cast({restart_logger_trace, Module, Line, Message}, State) ->
    handle_restart_logger_trace(Module, Line, Message, State);
handle_cast({restart_logger_stop, Module, Line}, State) ->
    handle_restart_logger_stop(Module, Line, State);
handle_cast(_, State) ->
    {noreply, State}.

handle_info(diskstats, State) ->
    case sysEnv:rcs_mode() of
    target ->
        {ok, Bin} = file:read_file("/sys/block/sda/stat"),
        Tokens = string:tokens(binary_to_list(Bin), " \n"),
        Keys = [reads,read_merges,read_sectors,read_ticks,
            writes,write_merges,write_sectors,write_ticks,
            in_flight,io_ticks,time_in_queue],
        Data = lists:zip(Keys, [list_to_integer(X)||X<-Tokens]),
        io:format(get(fd), "== ~w =============~n",[erlang:localtime()]),
        [case Key of
         in_flight ->
             io:format(get(fd), "~-15.15._w~10.10._w~n",[Key, Current]);
         _ ->
             Past = case ets:lookup(diskstats, Key) of
                [{_,P}] -> P;
                _ -> 0
                end,
             io:format(get(fd), "~-15.15._w~10.10._w~n",[Key, Current-Past]),
             ets:insert(diskstats, {Key, Current})
         end||{Key, Current}<-Data],

        ok;
    _ ->
        ok
    end,
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(Reason, State) ->
    handle_terminate(Reason, State),
    ok.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Initialize mnesia
%%% Search for a mnesia backup in $RCS_ROOT/home/$USER/restore/mnesia_backup
%%% @end
%%% ----------------------------------------------------------
prep_mnesia(MpId) ->
    case filelib:is_file(filename:join(home_dir(), "install_complete")) of
    true ->
        prep_mnesia_normal(MpId, sysEnv:rcs_mode_2());
    false ->
        prep_mnesia_for_install()
    end.

prep_mnesia_normal(1, vrcs) ->
    {ok, MnesiaDir} = application:get_env(mnesia, dir),
    Fallback = filename:join(MnesiaDir, "FALLBACK.BUP"),
    RestoreDir = filename:join(sysEnv:home_dir(), "restore"),
    case filelib:is_dir(RestoreDir) of
    true -> 
        %% This is a restore. Copy the backup db 
        BuPathGz = filename:join([RestoreDir, "mnesia_backup.gz"]),
        ok = filelib:ensure_dir(Fallback),
        os:cmd(["gunzip -c ",BuPathGz," > ", Fallback]);
    _ -> 
        %% This is a normal startup
        %% Use the autobackup file

        AutoGz = filename:join(sysEnv:home_dir(), "autobackup.gz"),
        ok = filelib:ensure_dir(Fallback),
        case certI:is_encryption_enabled() of
            true -> 
                TmpAutoGz = filename:join(sysEnv:tmp_dir(), "tmpautobackup.gz"),
                file:copy(AutoGz, TmpAutoGz),
                decrypt_db(TmpAutoGz),
                Res = os:cmd(["gunzip -c ", TmpAutoGz, " > ", Fallback]),
		sanity_test(AutoGz, TmpAutoGz, Fallback, Res),
		sysInitI:info_msg("~w: Deleting: ~p~n",[?MODULE,TmpAutoGz]),
		file:delete(TmpAutoGz);
            false ->
                Res = os:cmd(["gunzip -c ",AutoGz, " > ", Fallback]),
		sanity_test(AutoGz, AutoGz, Fallback, Res)
        end
    end;
prep_mnesia_normal(1, _) ->
    {ok, MnesiaDir} = application:get_env(mnesia, dir),
    AutoGz = filename:join(sysEnv:home_dir(), "autobackup.gz"),
    TmpPath = filename:join(sysEnv:tmp_dir(), "autobackup"),
    Fallback = filename:join(MnesiaDir, "FALLBACK.BUP"),
    case filelib:is_file(Fallback) of
    true ->
        %% In case of backup restore, make_release.escript will have
        %% placed a file here
        ok;
    false ->
        %% Otherwise use the autobackup.gz file
        ok = filelib:ensure_dir(Fallback),
        os:cmd(["gunzip -c ",AutoGz," > ", TmpPath]),
        file:make_symlink(TmpPath, Fallback) 
    end;
prep_mnesia_normal(_,_) ->
    %% Regular node, do nothing
    ok.

sanity_test(Tmp1, Tmp2, Fallback, Res) ->
    Options = [{file, Fallback},
	       {name, make_ref()},
	       {repair, false},
	       {mode, read_only}],
    case disk_log:open(Options) of
	{ok, Log} ->
	    disk_log:close(Log),
	    ok;
	Fck ->
	    sysInitI:error_msg("~w:Error while checking ~p~n"
			       "~p~n"
			       "Encrypted: ~s~n~s~n"
			       "Decrypted: ~s~n~s~n"
			       "gunzip: ~s~n"
			       "md5sum:~n~s~n"
			       "Head: ~n~s~n",
			       [?MODULE, Fallback, Fck,
				Tmp1, os:cmd("ls -l "++Tmp1),
				Tmp2, os:cmd("ls -l "++Tmp2),
				Res,
				os:cmd(["md5sum ", Fallback, " ", 
					Tmp1, " ", Tmp2]),
				os:cmd("head -10 "++Fallback)
			       ])
    end.

prep_mnesia_for_install() ->
    os:cmd(["rm -rf ", mnesia_dir()++"/*"]),
    os:cmd(["rm -rf ", mnesia_tmp_dir()++"/*"]),
    case sysEnv:role() of
    active ->
        filelib:ensure_dir(filename:join(mnesia_tmp_dir(), "x")),
        application:set_env(mnesia, dir, mnesia_tmp_dir()),
        mnesia:create_schema([node()]);
    _ ->
        ok
    end.

decrypt_db(Path) ->
    try
        {ok, Bin} = file:read_file(Path),
        case try_binary_to_term(Bin) of
            {encrypted, EncryptedData} ->
                {ok, DecryptedData} = certI:decrypt_data(EncryptedData),
                file:write_file(Path, DecryptedData),
                ok;
            _ ->
                %%In case db is not encrypted
                ok
        end
    catch Type:Reason ->
              warning([{?MODULE,{"Decryption of database failed ",
                                 [{Type, Reason}]}}])
    end.

try_binary_to_term(Bin) ->
    try
        binary_to_term(Bin)
    catch _:_ ->
        %%Not encrypted
        ok
    end.

%%% ----------------------------------------------------------
%%% @doc Return the home directory of the user $RCS_ROOT/home/$USER
%%% Cannot use sysEnv here because that module is not loaded
%%% @end
%%% ----------------------------------------------------------
home_dir() ->
    filename:join([os:getenv("RCS_ROOT"), "home", os:getenv("USER")]).

mnesia_dir() ->
    filename:join(home_dir(), "mnesia").

tmp_dir() ->
    filename:join(os:getenv("RCS_ROOT"), "tmp").

mnesia_tmp_dir() ->
    filename:join(tmp_dir(), "mnesia").

-spec init_restart_logger_port(vc_state()) -> port() | undefined.

init_restart_logger_port(VcState) ->
    case sysEnv:find_private_binary(
       sys, ?RESTART_LOGGER_PORT_PROGRAM, "NONE", VcState) of
    "NONE" ->
        warning([{?MODULE, {not_found, ?RESTART_LOGGER_PORT_PROGRAM}}]),
        undefined;
    Executable ->
        try
        open_port(
          {spawn_executable, Executable},
          [{packet, 2}, binary, {env, ?RESTART_LOGGER_DEBUG_DIR}]) 
        catch
        ExType:ExData ->
            warning(
              [{?MODULE,
            {open_port_failed, Executable, ExType, ExData}}]),
            undefined
        end
    end.

handle_restart_logger_trace(Module, Line, Message, #state{restart_logger_port=undefined}=State) ->
    warning({?MODULE, {"restart logger not available", Module, Line, Message}}),
    {noreply, State};

handle_restart_logger_trace(Module, Line, Message, #state{restart_logger_port=Port}=State) ->
    case erlang:port_info(Port) of
    undefined ->
        warning({?MODULE, {restart_logger_port_has_died, Module, Line, Message}}),
        {noreply, State#state{restart_logger_port=undefined}};
    _ ->
        SourceLoc = atom_to_list(Module)++".erl:"++integer_to_list(Line),
        Binary = twoStrings(SourceLoc, Message),
        Port ! {self(), {command, Binary}},
        info({?MODULE, {"restart event logged", SourceLoc, Message}}),
        receive
        {_Port, {data, <<"ok">>}} ->
            ok
        after ?RESTART_LOGGER_TIMEOUT ->
            ok
        end,
        {noreply, State}
    end.

%%% ----------------------------------------------------------
%%% @doc Pack two strings into a binary. The given strings are
%%% truncated to length 999 at most. Each string is preceded by
%%% a 3-character length field, containing the length encoded
%%% as a 3-digit zero-padded decimal number.
%%% @end
%%% ----------------------------------------------------------
-spec twoStrings(string(), string()) -> binary().

twoStrings(A, B) ->
    list_to_binary(oneString(A)++oneString(B)).

oneString(String) ->
    StringBounded = string:substr(String, 1, 999),
    Length = lists:flatten(io_lib:format("~3..0w", [length(StringBounded)])),
    Length++StringBounded.

handle_restart_logger_stop(Module, Line, #state{restart_logger_port=Port}=State) ->
    info({?MODULE, {"restart logger, stopped", Module, Line}}),
    stop_port(Port),
    {noreply, State#state{restart_logger_port=undefined}}.


handle_terminate(_Reason, #state{restart_logger_port=Port}) ->
    stop_port(Port).


stop_port(undefined) ->
    ?STOP_VERBOSE("port not active (1), no 'stop' action"),
    ok;

stop_port(Port) ->
    case erlang:port_info(Port) of
    undefined ->
        ?STOP_VERBOSE("port not active (2), no 'stop' action"),
        ok;
    _ ->
        Port ! {self(), close},
        receive
        {_Port, closed} ->
            ?STOP_VERBOSE("port closed"),
            ok
        after ?RESTART_LOGGER_TIMEOUT ->
            ?STOP_VERBOSE("closing port: timeout"),
            ok
        end
    end.

is_message_to_big(Format, Msg) ->
    Len = erlang:size(erlang:term_to_binary(Msg))+erlang:size(erlang:term_to_binary(Format)),
    if Len > ?SYS_MAX_MSG_SIZE ->
        true;
       true ->
        false
    end.

is_report_too_big(Report) ->
    Len = erlang:size(erlang:term_to_binary(Report)),
    if Len > ?SYS_MAX_MSG_SIZE ->
        true;
       true ->
        false
    end.

is_allowed_to_log(Mfa) ->
    ResetStat = {Mfa, get_time(), 0, 0, idle, 1},
    case ets:lookup(sysErrorLoggerStats, Mfa) of
    [] ->
        ets:insert(sysErrorLoggerStats, ResetStat),
        true;
    [CurrentStats]  ->
        Thresholds = ets:tab2list(sysErrorLoggerThresholds),
        MessageLimit = proplists:get_value(messageLimit, Thresholds, 20),
        TimeLimit = proplists:get_value(timeLimit, Thresholds, 15),
        HoldTime = proplists:get_value(holdTime, Thresholds, 60),
        QuarantineTime =
        proplists:get_value(quarantineTime, Thresholds, 60*20),
        case check_rules(CurrentStats, MessageLimit, TimeLimit) of
        go ->
            ets:update_counter(sysErrorLoggerStats, Mfa, {6, 1}),
            true;
        go_and_reset ->
            ets:insert(sysErrorLoggerStats, ResetStat),
            true;
        banned ->
            false;
        quarantine ->
            handle_quarantine(CurrentStats, MessageLimit, HoldTime,
                      QuarantineTime),
            false;
        deny_print ->
            handle_deny_print(CurrentStats, HoldTime, QuarantineTime),
            false
        end
    end.

get_time() ->
    erlang:monotonic_time(seconds).

check_rules({_, FirstPrintTime, BannedUntilTime, QuarantineUntilTime, _,
         MessageRequests}, MessageLimit, TimeLimit)
  when MessageRequests >= MessageLimit ->
    CurrentTime = get_time(),
    if BannedUntilTime > CurrentTime ->
        banned;
       QuarantineUntilTime > CurrentTime ->
        quarantine;
       (CurrentTime - FirstPrintTime) < TimeLimit ->
        deny_print;
       (CurrentTime - FirstPrintTime) > TimeLimit ->
        go_and_reset;
       true ->
        go
    end;
check_rules(_, _, _) ->
    go.

handle_quarantine(CurrentStats, MessageLimit, HoldTime, QuarantineTime) ->
    {Mfa, FirstPrintTime, _, _, QuarantineStatus, MessageRequests} =
    CurrentStats,
    Now = get_time(),
    BanningPeriod = HoldTime + QuarantineTime,
    {NewFp, NewMR} =
    case QuarantineStatus of
        in_progress ->
        if
            %% Issue a status report each 60 secs
            FirstPrintTime + 60 < Now ->
            force_info_msg(
              "~w blocked messages in ~w seconds from~n   ~p~n"
              "while in quarantine.~n"
              "Next printout will be allowed in ~w seconds~n",
              [MessageRequests - MessageLimit,
               Now-FirstPrintTime, Mfa, BanningPeriod]),
            {Now, MessageLimit};
            true ->
            {FirstPrintTime, MessageRequests}
        end;
        idle ->
        force_info_msg(
          "Quarantining ~n   ~p~n"
          "because it is still making printouts after "
          "the ~w seconds burst prevention period~n"
          "Next printout will be allowed in ~w seconds~n",
         [Mfa, HoldTime, BanningPeriod]),
        {Now, MessageRequests}
    end,

    ets:insert(sysErrorLoggerStats,
           {Mfa,
        NewFp,
        0,
        Now + BanningPeriod,
        in_progress,
        NewMR+1}).

handle_deny_print(CurrentStats, HoldTime, QuarantineTime) ->
    {Mfa, FirstPrintTime, _, _, _, MessageRequests} = CurrentStats,
    Now = get_time(),
    force_info_msg(
      "Banning printouts from~n   ~p~n"
      "for making ~w printouts in the last ~w seconds~n"
      "Next printout is allowed in ~w seconds.~n",
      [Mfa, MessageRequests, Now-FirstPrintTime, HoldTime+QuarantineTime]),
    ets:insert(sysErrorLoggerStats,
           {Mfa,
        FirstPrintTime,
        Now + HoldTime,
        Now + HoldTime + QuarantineTime,
        idle,
        MessageRequests}).

lift_quarantine() ->
    ets:delete_all_objects(sysErrorLoggerStats).

lift_quarantine(Module) ->
    Pattern = {{Module,'$1','$2','$3'}, '_', '_', '_', '_', '_'},
    Objs = ets:match(sysErrorLoggerStats, Pattern),
    [ets:delete(sysErrorLoggerStats, {Module, F, Arity, Location})||
    [F, Arity, Location] <- Objs],
    sysInitI:info_msg("Removed ~w sysErrorLogger stat entries",[length(Objs)]).

info(T) ->
    sysInitI:info_report([T]).

warning(T) ->
    sysInitI:warning_report([T]).

error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

read_vc_state(false) ->
    unknown;
read_vc_state(CupBinary) ->
    Port = open_port({spawn_executable, CupBinary},
            [stream, {cd, "/tmp"}, {args, ["-c"]}, binary,
             stderr_to_stdout, exit_status]),
    read_port_result(Port).

read_port_result(Port) ->
    receive
    {Port,{exit_status, 0}} -> vc;
    {Port,{exit_status, _}} -> no_vc;
    {Port, _} -> read_port_result(Port)
    after
    5000 ->
        catch port_close(Port),
        error_msg("~s~n", ["Timeout reading VC status"]),
        unknown
    end.

return_lmt_ns() ->
    File = filename:join(?NS_PATH, ?LMT_NS),
    case file:read_file_info(File, [raw, {time, posix}]) of
    {ok, _} -> ?LMT_NS;
    {error, _} -> <<>>
    end.

cover_compile(true, "true", Apps) ->
    error_logger:info_msg("~w: cover compiling~n", [?MODULE]),
    sysDebug:cover_compile(Apps),
    ok;
cover_compile(_, _, _) ->
    ok.

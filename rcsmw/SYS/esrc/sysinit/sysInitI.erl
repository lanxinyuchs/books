%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysInitI.erl %
%%% @author etxpejn
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R9A/R12A/1

%%% @doc ==System startup interface==
%%% The sysinit application is the first application to start up and the last
%%% to be taken down
%%% @end

-module(sysInitI).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R9A/R12A/1').
-date('2017-11-08').
-author('etxpejn').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
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
%%%    .... Removed older history .....
%%% R4A/1      2015-08-27 etxjotj     Error logger interface
%%% R4A/3      2015-09-17 etxjotj     ETS adoption
%%% ----------------------------------------------------------
%%% R5A/1      2015-10-06 etxjotj     Disk log
%%% R5A/2      2015-11-02 etxtory     Added *_msg; so regular can
%%%                                   can print on core node
%%% R5A/6      2015-11-19 etxpeno     Add log_restart_time/0
%%% R5A/7      2015-12-09 etxpeno     store the timestamps in sysInitApp
%%%                                   in microseconds
%%% R5A/8      2016-01-22 etxtory     Save erlang.log to erlang_disk
%%% R5A/9      2016-02-02 etxarnu     Corrected erlang_disk dir for sim
%%% R5A/10     2016-02-05 etxarnu     Changed error_msg to info_msg in get_save_dir
%%% R5A/11     2016-03-12 erarafo     Added is_secure/1
%%% R6A/1      2016-07-08 uabesvi     fixed get_function
%%% R9A/1      2017-03-24 etomist     HV73396
%%% R12A/1     2017-11-08 etxpejn     Changed get_stacktrace order in get_function
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([error_report/1,
	 error_report/2,
	 info_report/1,
	 info_report/2,
	 warning_report/1,
	 warning_report/2,
	 error_msg/1,
	 error_msg/2, error_msg_node/3,
	 info_msg/1,
	 info_msg/2, info_msg_node/3,
	 warning_msg/1,
	 warning_msg/2, warning_msg_node/3]).

-export([restart_type/0,
	 restart_logger_trace/3,
	 restart_logger_stop/2
	]).

-export([set_watchdog_interval/1, reset_watchdog_interval/0,
	 disable_watchdog/0, enable_watchdog/0]).

-export([is_secure/0, is_secure/1]).
-export([get_lmt_ns/0]).

-export([log_startup_time/1, log_restart_time/0]).

-export([ets_new/2, ets_delete/1]).

-export([save_logs/0]).

-export([get_logger_depth/0]).


%% Includes
-include_lib("kernel/include/file.hrl").
-include("sys.hrl").

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Distinguishes between a local restart and an erlang vm restart
%%% @end
%%% ----------------------------------------------------------

-spec restart_type() -> local | vm_restart.

restart_type() ->
    sysInitServer:restart_type().

%%% ----------------------------------------------------------
%%% @doc Write a trace entry at restart.
%%% @end
%%% ----------------------------------------------------------

-spec restart_logger_trace(atom(), integer(), string()) -> ok.

restart_logger_trace(Module, Line, Message) ->
    sysInitServer:restart_logger_trace(Module, Line, Message).

%%% ----------------------------------------------------------
%%% @doc Stop the restart trace service.
%%% @end
%%% ----------------------------------------------------------

-spec restart_logger_stop(atom(), integer) -> ok.

restart_logger_stop(Module, Line) ->
    sysInitServer:restart_logger_stop(Module, Line).

%%% ----------------------------------------------------------
%%% @doc Set the watchdog interval
%%% @end
%%% ----------------------------------------------------------

-spec set_watchdog_interval(Interval::non_neg_integer()) -> ok.

set_watchdog_interval(Interval) when Interval > 0 ->
    gen_server:call(sysInitServer, {set_wd_interval, Interval}).

%%% ----------------------------------------------------------
%%% @doc Reset the watchdog interval
%%% @end
%%% ----------------------------------------------------------

-spec reset_watchdog_interval() -> ok.

reset_watchdog_interval() ->
    gen_server:call(sysInitServer, unset_wd_interval).

%%% ----------------------------------------------------------
%%% @doc Disable the watchdog (for testing purposes)
%%% @end
%%% ----------------------------------------------------------

-spec disable_watchdog() -> disabled.

disable_watchdog() ->
    gen_server:call(sysInitServer, disable_wd).

%%% ----------------------------------------------------------
%%% @doc Reenable the watchdog (for testing purposes)
%%% @end
%%% ----------------------------------------------------------

-spec enable_watchdog() -> enabled | already_enabled.

enable_watchdog() ->
    gen_server:call(sysInitServer, enable_wd).

%%% ----------------------------------------------------------
%%% @doc Test if board has vendor credentials installed
%%% Returns 'true' if the boards has vendor credentials on it which means
%%% no lab interfaces are open.
%%% @end
%%% ----------------------------------------------------------

-spec is_secure() -> boolean().

is_secure() ->
    sysInitServer:is_secure().

%%% ----------------------------------------------------------
%%% @doc Same as is_secure/0 when vc_state is available.
%%% @end
%%% ----------------------------------------------------------

-spec is_secure(vc_state()) -> boolean().

is_secure(vc) ->
    true;
is_secure(_VcState) ->
    false.

%%% ----------------------------------------------------------
%%% @doc Return the network namespace of the LMT port
%%% @end
%%% ----------------------------------------------------------

-spec get_lmt_ns() -> binary().

get_lmt_ns() ->
    sysInitServer:get_lmt_ns().

%%-----------------------------------------------------------------
%% This functions should be used for error reports.  Events
%% are tagged 'error_report'.
%% The 'std_error' error_report type can always be used.
%%-----------------------------------------------------------------

-type report() ::
        [{Tag :: term(), Data :: term()} | term()] | string() | term().

-spec error_report(Report) -> 'ok' when
      Report :: report().

error_report(Report) ->
    Mfa = get_function(),
    sysInitServer:make_report({error, Mfa, std_error, Report}).

-spec error_report(Type, Report) -> 'ok' when
      Type :: term(),
      Report :: report().

error_report(Type, Report) ->
    Mfa = get_function(),
    sysInitServer:make_report({error, Mfa, Type, Report}).

%%-----------------------------------------------------------------
%% These two simple old functions generate events tagged 'error'
%% Used for simple messages; error or information.
%%-----------------------------------------------------------------

-spec error_msg(Format) -> 'ok' when
      Format :: string().

error_msg(Format) ->
    Mfa = get_function(),
    sysInitServer:make_msg({error, Mfa, Format, []}).

-spec error_msg(Format, Data) -> 'ok' when
      Format :: string(),
      Data :: list().

error_msg(Format, Data) ->
    Mfa = get_function(),
    sysInitServer:make_msg({error, Mfa, Format, Data}).

-spec error_msg_node(Node, Format, Data) -> 'ok' when
      Node :: node(),
      Format :: string(),
      Data :: list().

error_msg_node(Node, Format, Data) ->
    sysInitServer:error_msg_node(Node, Format, Data).

%%-----------------------------------------------------------------
%% This function should be used for information reports.  Events
%% are tagged 'info_report'.
%% The 'std_info' info_report type can always be used.
%%-----------------------------------------------------------------

-spec info_report(Report) -> 'ok' when
      Report :: report().

info_report(Report) ->
    Mfa = get_function(),
    sysInitServer:make_report({info, Mfa, std_info, Report}).

-spec info_report(Type, Report) -> 'ok' when
      Type :: any(),
      Report :: report().

info_report(Type, Report) ->
    Mfa = get_function(),
    sysInitServer:make_report({info, Mfa, Type, Report}).

%%-----------------------------------------------------------------
%% This function provides similar functions as error_msg for
%% information messages.
%%-----------------------------------------------------------------

-spec info_msg(Format) -> 'ok' when
      Format :: string().

info_msg(Format) ->
    Mfa = get_function(),
    sysInitServer:make_msg({info, Mfa, Format, []}).

-spec info_msg(Format, Data) -> 'ok' when
      Format :: string(),
      Data :: list().

info_msg(Format, Data) ->
    Mfa = get_function(),
    sysInitServer:make_msg({info, Mfa, Format, Data}).

-spec info_msg_node(Node, Format, Data) -> 'ok' when
      Node :: node(),
      Format :: string(),
      Data :: list().

info_msg_node(Node, Format, Data) ->
    sysInitServer:info_msg_node(Node, Format, Data).

%%-----------------------------------------------------------------
%% This function should be used for warning reports.
%% These might be mapped to error reports or info reports,
%% depending on emulator flags. Events that ore not mapped
%% are tagged 'info_report'.
%% The 'std_warning' info_report type can always be used and is
%% mapped to std_info or std_error accordingly.
%%-----------------------------------------------------------------

-spec warning_report(Report) -> 'ok' when
      Report :: report().

warning_report(Report) ->
    Mfa = get_function(),
    sysInitServer:make_report({warning, Mfa, std_warning, Report}).

-spec warning_report(Type, Report) -> 'ok' when
      Type :: any(),
      Report :: report().

warning_report(Type, Report) ->
    Mfa = get_function(),
    sysInitServer:make_report({warning, Mfa, Type, Report}).

%%-----------------------------------------------------------------
%% This function provides similar functions as error_msg for
%% warning messages, like warning report it might get mapped to
%% other types of reports.
%%-----------------------------------------------------------------

-spec warning_msg(Format) -> 'ok' when
      Format :: string().

warning_msg(Format) ->
    Mfa = get_function(),
    sysInitServer:make_msg({warning, Mfa, Format, []}).

-spec warning_msg(Format, Data) -> 'ok' when
      Format :: string(),
      Data :: list().

warning_msg(Format, Data) ->
    Mfa = get_function(),
    sysInitServer:make_msg({warning, Mfa, Format, Data}).

-spec warning_msg_node(Node, Format, Data) -> 'ok' when
      Node :: node(),
      Format :: string(),
      Data :: list().

warning_msg_node(Node, Format, Data) ->
    sysInitServer:warning_msg_node(Node, Format, Data).

get_function() ->
    try erlang:error(nofault, [])
    catch _:_ ->
	    Stacktrace = erlang:get_stacktrace(),
	    gf(Stacktrace)
    end.

gf([_, _, B|_]) ->
    B;
gf([_, B|_]) ->
    B;
gf([B|_]) ->
    B.


%%% ----------------------------------------------------------
%%% @doc Log timestamps for startup time logging
%%% @end
%%% ----------------------------------------------------------

-spec log_startup_time(Tag::term()) -> ok.

log_startup_time(Tag) ->
    TS = os:system_time(micro_seconds),
    ets:insert(sysInitApp, {Tag, ts, TS}),

    Starts = ets:lookup(sysInitApp, sysInit_start),

    {_, _, TStart} = lists:keyfind(ts, 2, Starts),
    ElapsedTs = (TS-TStart)/1000000,

    BeamStarts = ets:lookup(sysInitApp, beam_start),
    OtpStart =
	case lists:keyfind(ts, 2, BeamStarts) of
	    {_, _, BeamStart} ->
		(TStart-BeamStart)/1000000;
	    _ ->
		undefined
	end,

    MwStart =
	case ets:lookup(sysInitApp, applications_starting) of
	    [] when Tag == applications_starting ->
		(TS-TStart)/1000000;
	    [] ->
		undefined;
	    AppStarts1 ->
		case lists:keyfind(ts, 2, AppStarts1) of
		    {_, _, AS1} ->
			(AS1-TStart)/1000000;
		    _ ->
			undefined
		end
	end,


    AppStart =
	case ets:lookup(sysInitApp, applications_starting) of
	    [] ->
		undefined;
	    AppStarts ->
		case lists:keyfind(ts, 2, AppStarts) of
		    {_, _, AS} ->
			(TS-AS)/1000000;
		    _ ->
			undefined
		end
	end,

    FullRestart =
	case ets:lookup(sysInitApp, restart_piu) of
	    [{_, _, RestartPiu}] ->
		(TS-RestartPiu)/1000000;
	    _ ->
		undefined
	end,

    GmfStart =
	case ets:lookup(sysInitApp, gmf_starting) of
	    [] ->
		undefined;
	    GmfStarts ->
		case lists:keyfind(ts, 2, GmfStarts) of
		    {_, _, GS} ->
			(TS-GS)/1000000;
		    _ ->
			undefined
		end
	end,

    sysInitI:info_report([{mfa, {?MODULE, log_startup_time, [Tag]}},
			  {elapsed_ts, ElapsedTs},
			  {otp_start, OtpStart},
			  {full_restart, FullRestart},
			  {application_start, AppStart},
			  {mw_start, MwStart},
			  {gmf_start, GmfStart}
			 ]),
    ok.

%%% ----------------------------------------------------------
%%% @doc Log timestamp at restart of piu
%%% @end
%%% ----------------------------------------------------------

-spec log_restart_time() -> ok.

log_restart_time() ->
    sysInitApp:write_shutdown_data(),
    ok.

%%% ----------------------------------------------------------
%%% @doc Let sysInitI be the owner of an ets table
%%% @end
%%% ----------------------------------------------------------

-spec ets_new(Tab::atom(), Opts::list()) -> {ok, term()}|tuple().

ets_new(Tab, Opts) ->
    sysInitEts:ets_new(Tab, Opts).

%%% ----------------------------------------------------------
%%% @doc Remove an ets table that sysInit owns
%%% @end
%%% ----------------------------------------------------------

-spec ets_delete(Tab::atom()) -> true | tuple().

ets_delete(Tab) ->
    sysInitEts:ets_delete(Tab).

%%% ----------------------------------------------------------
%%% @doc Saves /rcs/erlang/ (pramfs) to persistent disk. 
%%% Only handled for target environment. 
%%% @end
%%% ----------------------------------------------------------

-spec save_logs() -> ok.

save_logs() ->
    save_logs(sysEnv:target()).

save_logs(_Target = true) ->
    case get_save_dir() of
	{ok, SaveDir} ->
	    ErlangDir = sysEnv:rcs_root() ++ "rcs/erlang/",
	    os:cmd("rm -rf " ++ SaveDir),
	    file:make_dir(SaveDir),
	    SaveFile = filename:join([SaveDir, "erlang.tar.gz"]),
	    os:cmd("cd " ++ ErlangDir ++ "; " 
		   ++ "tar cvfz " ++ SaveFile ++ " *"),
	    ok;
	nok ->
	    ok
    end;
save_logs(_Target = false) ->
    ok.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
get_logger_depth() ->
    case application:get_env(kernel, error_logger_format_depth) of
        {ok, X} when is_integer(X) -> X;
        _ -> unlimited
    end.
%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% Save logs functions
%%% ----------------------------------------------------------
get_save_dir() ->
    DiskDir = filename:join([sysEnv:rcs_dir(), "erlang_disk"]),
    case file:list_dir(DiskDir) of
	{ok, DiskDirList} ->
	    SaveDir = get_save_dir(DiskDir, DiskDirList, _Acc = []),
	    {ok, SaveDir};
	{error, Reason} ->
	    info_msg("Listing files in ~p failed ~p~n", [DiskDir, Reason]),
	    nok
    end.

get_save_dir(DiskDir, [], Acc) ->
    Len = lists:flatlength(Acc),
    Index = get_index(Len, Acc),
    filename:join([DiskDir, "save_log_" ++ Index]);
get_save_dir(DiskDir, [SaveDir = "save_log_" ++ _Index | T], Acc) ->
    FullDir = filename:join([DiskDir, SaveDir]),
    case file:read_file_info(FullDir) of
	{ok, FI} ->
	    Element = {FI#file_info.ctime, FullDir},
	    get_save_dir(DiskDir, T, [Element | Acc]);
	{error, Reason} ->
	    error_msg("Reading file_info for ~p failed ~p~n", [FullDir, Reason]),
	    get_save_dir(DiskDir, T, Acc)
    end;
get_save_dir(DiskDir, [_ | T], Acc) ->
    get_save_dir(DiskDir, T, Acc).

get_index(_Len = 0, _SaveLogList = []) ->
    %% First time, Index = "1"
    "1";
get_index(Len, _SaveLogList) when Len > 5 ->
    %% Not passed 5 anytime; Index "2" to "5"
    integer_to_list(Len + 1);
get_index(_Len, SaveLogList) ->
    %% Passed 5 sometime; find the newest and then + 1
    SortedSaveLogList = lists:keysort(1, SaveLogList),
    {_Ctime, SaveDir} = lists:last(SortedSaveLogList),
    Index = lists:last(SaveDir),
    case list_to_integer([Index]) of
	5 -> integer_to_list(1);
	No -> integer_to_list(No + 1)	      
    end.

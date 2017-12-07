%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysInitApp.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R7A/R9A/R11A/1

%%% @doc ==Application module for sysInit==
-module(sysInitApp).
-behaviour(application).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R7A/R9A/R11A/1').
-date('2017-10-17').
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
%%%    .... Removed older history .....
%%% R5A/1      2015-10-07 etxjotj     OTP 18 adaptions
%%% R5A/6      2015-11-19 etxpeno     add write_shutdown_data/0
%%% R5A/7      2015-12-09 etxpeno     store the timestamps in sysInitApp
%%%                                   in microseconds
%%% R5A/8      2016-03-02 etxarnu     Print OTP version
%%% R5A/9      2016-03-18 etxarnu     Improved OTP version 
%%% R5A/10     2016-04-12 etxarnu     Changed os:putenv to ets table for warm_cnt
%%% R5A/11     2016-04-13 etxtory     Fixed dialyze error io_lib:format
%%% -----   -------    --------   ------------------------
%%% R6A/1      2016-09-20 etxberb     Correction of OTP version logging
%%% -----   -------    --------   ------------------------
%%% R9A/1      2017-04-04 etxarnu     Added cold restart counter
%%% -----   -------    --------   ------------------------
%%% R11A/1  2017-10-17 etxberb    Adaptions to OTP20; Removed export_all.
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([start/2, start_phase/3, prep_stop/1, stop/1, config_change/3]).

-export([is_cold_restart/0]).
-export([reset_warm_cnt/0]).
-export([inc_warm_cnt/0]).
-export([print_warm_cnt/0]).
-export([is_escalation_enabled/0]).
-export([write_shutdown_data/0]).


%% Next functions are used in VRCS to stop cyclic restarts
-define(MAX_NO_COLD_RESTARTS, 5).       % 5 restarts in
-define(COLD_RESTART_LIMIT, (8 * 60) ). % 8 minutes

-export([inc_cold_cnt/0]).
-export([is_cyclic_restart/0]).

-export([reset_cold_cnt/0]).
%for test
-export([get_cold/0]).
-export([remove_old_restarts/1]).



%%% COLI commands
-export([disable_escalation/0, disable_escalation/1]).
-export([enable_escalation/0, enable_escalation/1]).
-export([restart/1,reboot/1]).

-compile([nowarn_unused_vars]).
%%%-compile([nowarn_unused_vars, export_all]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
-define(ESCALATE_RESTART_FILE,
	filename:join(sysEnv:home_dir(),"do_not_escalate_restart")).


%%% Originally called directly from COLI with a list of arguments. After
%%% restructuring of COLI commands, this function is called indirectly from a
%%% COLI command handling function in sysColi. Thus making the argument to this
%%% function obsolete (undefined).
enable_escalation() ->
    enable_escalation(undefined).

enable_escalation(_) ->
    os:cmd("rm " ++ ?ESCALATE_RESTART_FILE),
    error_logger:info_msg("sysInitApp:enable_escalation ~n",[]).

%%% Originally called directly from COLI with a list of arguments. After
%%% restructuring of COLI commands, this function is called indirectly from a
%%% COLI command handling function in sysColi. Thus making the argument to this
%%% function obsolete (undefined).
disable_escalation() ->
    disable_escalation(undefined).

disable_escalation(_) ->
    os:cmd("touch " ++ ?ESCALATE_RESTART_FILE),
    error_logger:info_msg("sysInitApp:disable_escalation ~n",[]).

is_escalation_enabled() ->
    case file:read_file_info(?ESCALATE_RESTART_FILE) of
	{ok,_} ->
	    false;
	_ ->
	    true
    end.

%%% Originally intended for COLI command. After restructuring of COLI commands,
%%% this function is no longer used by COLI. May be used by others though...
restart(Args) ->
    error_logger:info_msg("sysInitApp:restart(~p)~n", [Args]),
    appmI:restart_piu_warm(Args).
%%% Originally intended for COLI command. After restructuring of COLI commands,
%%% this function is no longer used by COLI. May be used by others though...
reboot(Args) ->
    error_logger:info_msg("sysInitApp:reboot(~p)~n", [Args]),
    appmI:restart_piu_cold(Args).

start(_StartType, _StartArgs) ->
    TS = os:system_time(micro_seconds),
    ets:new(sysInitApp, [public, bag, named_table]),
    ets:new(sysInitApp_warmcnt, [public, set, named_table]),
    ets:insert(sysInitApp, {sysInit_start, ts, TS}),
    proc_lib:spawn_opt(fun() -> try_parse_boot_log() end, [{priority, low}]),
    proc_lib:spawn_opt(fun() -> try_parse_shutdown_data() end, [{priority, low}]),
    inc_warm_cnt(),
    print_otp_version(),
    print_restart_reason(),
    print_warm_cnt(),
    case sysEnv:vrcs() of
	true -> %to handle cyclic vrcs restarts, done in appmServer
	    inc_cold_cnt(),
	    print_cold_cnt();
	false ->
	    ok
    end,

    supervisor:start_link({local, sysInitSuper}, sysInitSup, []).

try_parse_boot_log() ->
    try parse_boot_log()
    catch T:E ->
	    error_logger:warning_report([{mfa, {?MODULE, parse_boot_log, []}},
					 {T,E},
					 erlang:get_stacktrace()])
    end.

try_parse_shutdown_data() ->
    try parse_shutdown_data()
    catch T:E ->
	    Warning = [{mfa, {?MODULE, parse_shutdown_data, []}},
		       {T,E},
		       erlang:get_stacktrace()],
	    error_logger:warning_report(Warning)
    end.

parse_boot_log() ->
    case sysEnv:architecture() of
	{"arm",_} ->
	    Path = filename:join([sysEnv:rcs_dir(), "bootlogs",
				  "startup", "bootlog"]),
	    Res = os:cmd(["babeltrace ", Path]),
	    case Res of
		"[error]"++_ ->
		    error_logger:warning_msg("babeltrace ~s~n~s~n",
					     [Path, Res]);
		_ ->
		    Lines = string:tokens(Res, "\n"),
		    TS = extract_timestamp(hd(Lines)),
		    ets:insert(sysInitApp, {bootlog_start, ts, TS}),
		    case find_line(Lines,"rcs_start ready, starting erlang") of
			{error, no_such_line} ->
			    ok;
			Line ->
			    TS2 = extract_timestamp(Line),
			    ets:insert(sysInitApp, {beam_start, ts, TS}),
			    Starts = ets:lookup(sysInitApp, sysInit_start),
			    {_, _, NStart} = lists:keyfind(ts, 2, Starts),
			    ElapsedNow = (NStart-TS2)/1000000,

			    error_logger:info_msg("beam startup: ~w~n",
						  [ElapsedNow])
		    end
	    end;
	_ ->
	    ok
    end.

extract_timestamp(Line) ->
    {match, [{Start, Length}]} = re:run(Line, "\\[.*\\]"),
    Timestamp = string:substr(Line, Start+2, Length-2),
    {ok, [H,M,S,D], _} =
	case io_lib:fread("~d:~d:~d.~s", Timestamp) of
	    {ok, InputList, LeftOverChars} ->
		{ok, InputList, LeftOverChars};
	    {more, _, _, _} ->
		erlang:error(incomplete_timestamp, [Line]);
	    {error, {fread, What}} ->
		erlang:error({fread, What}, [Line])
	end,

    Dstr = string:left(D, 6, $0),
    {Day, Time} = erlang:universaltime(),
    Seconds =
	case {H,M,S} of
	    ThisTime when ThisTime > Time ->
		%% We've passed in to next day
		calendar:datetime_to_gregorian_seconds({Day, ThisTime})-
		    62167219200-86400;
	    ThisTime ->
		calendar:datetime_to_gregorian_seconds({Day, ThisTime})-
		    62167219200
	end,
    Seconds*1000000+list_to_integer(Dstr).

find_line([Line|Lines], Regexp) ->
    case re:run(Line, Regexp) of
	{match, [_]} ->
	    Line;
	nomatch ->
	    find_line(Lines, Regexp)
    end;
find_line([], _) ->
    {error, no_such_line}.

write_shutdown_data() ->
    LogPath = filename:join([sysEnv:rcs_dir(), "appm", "restart_piu.bin"]),
    filelib:ensure_dir(LogPath),
    file:write_file(LogPath, term_to_binary({restart_piu,
					     os:system_time(micro_seconds),
					     v2})).

parse_shutdown_data() ->
    Path = filename:join([sysEnv:rcs_dir(),"appm","restart_piu.bin"]),
    case file:read_file(Path) of
	{ok, Bin} ->
	    file:delete(Path),
	    case binary_to_term(Bin) of
		{restart_piu, TS, v2} ->
		    ets:insert(sysInitApp, {restart_piu, ts, TS});
		_ ->
		    ok
	    end;
	{error, enoent} ->
	    ok;
	{error, Reason} ->
	    file:delete(Path),
	    erlang:error(Reason, [])
    end.


start_phase(_Phase, _StartType, _PhaseArgs) ->
    ok.

prep_stop(State) ->
    State.

stop(_State) ->
    error_logger:info_report(progress, [{application, sysInit},
					{stopped_at, node()}]).

config_change(_Changed, _New, _Removed) ->
    ok.



%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------



%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

is_cold_restart() ->
    case restart_type() of
	cold ->
	    true;
	_ ->
	    false
    end.

restart_type() ->
    case get_warm() of
	false -> cold;
	List ->
	    case length(List) of
		1 ->
		    cold;
		2 ->
		    warm;
		_ ->
		    [Last|[Prev|_Rest]] = List,
		    L = list_to_integer(Last),
		    P = list_to_integer(Prev),
		    TimeSinceLastWarm = L - P,
		    case TimeSinceLastWarm > warm_restart_time_limit() of
			true ->
			    warm;
			false ->
			    escalate
		    end
	    end
    end.

inc_warm_cnt() ->
    case  get_warm() of
	false -> % After a cold restart
	    reset_warm_cnt(),
	    cold;
	Val ->
	    add_warm_cnt(Val)
    end.

reset_warm_cnt() ->
    put_warm([integer_to_list(os_now())]).

add_warm_cnt(Val) ->
    Now = os_now(),
    put_warm([integer_to_list(os_now())|Val]),
    Now.

get_warm() ->
    case ets:lookup(sysInitApp_warmcnt,ts) of
	[{ts, List}] -> 
	    List;
	_ ->
	    false
    end.

put_warm(Val) ->
        ets:insert(sysInitApp_warmcnt, {ts,Val}).

os_now() ->
    os:system_time(seconds).

print_otp_version() ->
    OtpRoot = os:getenv("OTP_ROOT"),
    VersionFile = filename:join([OtpRoot,"otp.version"]),
    case file:read_file(VersionFile) of
	{ok,Bin} ->
	    try
		begin
		    VersionStr = binary_to_list(Bin),
		    OTP_info = string:tokens(VersionStr, " -\n"),
		    OTP_info_str =
			"~nsysInitApp: OTP version info:~n" ++
			lists:flatten(["~s~n" || _ <- OTP_info]) ++
			"~n",
		    io:format(OTP_info_str, OTP_info)
		end
	    catch T:E ->
		    error_logger:warning_report(
		      [{mfa, {?MODULE, print_otp_version, []}},
		       {T,E},
		       erlang:get_stacktrace()])
	    end;
		     
	_ ->
	    ok
    end.

warm_restart_time_limit() ->
    %% 5 minutes
    5 * 60 .



print_restart_reason() ->
    case sysEnv:target() of
	true ->
	    error_logger:info_msg("sysInitApp:~n"
				  "Last EE restart reason: ~p~n"
				  "     EE bootcounter: ~p~n"
				  "     EE bootpointer: ~p~n",
				  [appmRhai:read("restart_type"),
				   sysRhai:getbootcounter(),
				   sysRhai:getbootptr()]);
	false ->
	    ok
    end.

print_warm_cnt() ->
    CntStr = pr_wrm(),
    error_logger:info_msg("sysInitApp: Last restarts:~n~p~n",[CntStr]),
    io:format("~s",["===END==="]).

pr_wrm() ->
    case get_warm() of
	false ->
	    io_lib:format("sysInitApp:print_cnters -- warm_cnt not set !", []);
	List ->
	    [pr_cnt(list_to_integer(L)) || L<-List]
    end.

pr_cnt(I) ->
    MS = I div 1000000,
    S = I rem 1000000,
    Str = comsaLib:iso_time({MS,S,0},extended),
    io_lib:format("~s",[Str]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
inc_cold_cnt() ->
    case  get_cold() of
	false -> % first time
	    reset_cold_cnt();
	Val ->
	    add_cold_cnt(Val)
    end.

reset_cold_cnt() ->
    put_cold([os_now()]).

add_cold_cnt(Val) ->
    put_cold([os_now()|Val]).

get_cold() ->
    case file:read_file(cold_restart_file()) of
	{ok,B} ->
	    binary_to_term(B);
	_ ->
	    false
    end.

put_cold(Val) ->
    file:write_file(cold_restart_file(),
		    term_to_binary(Val)).



is_cyclic_restart() ->
    case get_cold() of
	false -> false;
	List ->
	    L2 = remove_old_restarts(List),
	    case length(L2) of
		X when X < ?MAX_NO_COLD_RESTARTS ->
		    false;
		_ ->
		    true
	    end
    end.

cold_restart_file() ->
    filename:join([sysEnv:rcs_dir(),"sys","restart_cnt_log"]).

remove_old_restarts(L) ->
    remove_old_restarts(L,os_now(),[]).

remove_old_restarts([F|R],T,Acc) ->
    case (T-F) > ?COLD_RESTART_LIMIT of
	true ->
	    remove_old_restarts(R,T,Acc);
	false ->
	    remove_old_restarts(R,T,[F|Acc])
    end;
remove_old_restarts([],_T,Acc) ->
    L = lists:reverse(Acc),
    put_cold(L),
    L.
	
print_cold_cnt() ->
    CntStr = pr_cold(),
    error_logger:info_msg("sysInitApp: Last cold restarts:~n~p~n",[CntStr]),
    io:format("~s",["===END==="]).

pr_cold() ->
    case get_cold() of
	false ->
	    io_lib:format("sysInitApp:print_cnters -- cold_cnt not set !", []);
	List ->
	    [pr_cnt(L) || L<-List]
    end.
